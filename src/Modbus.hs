module Modbus (modSession) where

import Control.Monad.Except (runExceptT, throwError)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Network.Socket.ByteString (recv, send)
import Data.Binary.Put (runPut, putWord16le)
import Data.Binary.Get (runGet, getFloatbe, getFloatle)

import CsvParser (ModData (..), ModType (..), RegType (..), ByteOrder (..), modData)

import qualified Data.Text as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

modSession :: [ModData] -> ByteOrder -> MB.Session [ModData]
modSession md order =
  let 
    md' = zip [0 ..] md
  in 
    mapM (`genSession` order) md'
  
genSession :: (Word16, ModData) -> ByteOrder -> MB.Session ModData
genSession (idx, md) order =
  let 
    addr = MB.unRegAddress $ register md
    tp = regType md
  in 
    case tp of
        InputRegister -> readInputModData md idx order
        HoldingRegister -> readHoldingModData md idx order
        _ -> throwError $ MB.OtherException "Invalid function type requested"

readInputModData :: ModData -> Word16 -> ByteOrder -> MB.Session ModData
readInputModData md idx order = 
    case value md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readInputRegisters (MB.TransactionId idx) 0 255 reg_address 1
            return md {value = ModWord resp}
        ModFloat _ -> do
            xs <- MB.readInputRegisters (MB.TransactionId idx) 0 255 reg_address 2
            case xs of
                [msw,lsw] -> return md {value = ModFloat $ getFloat order (msw,lsw)} 
                _ -> throwError $ MB.OtherException $ 
                            "Error reading Float from input register address: "
                            ++ show reg_address
                            ++ ", "
                            ++ show (reg_address + 1)
  where 
    reg_address = register md
            
            
readHoldingModData :: ModData -> Word16 -> ByteOrder -> MB.Session ModData
readHoldingModData md idx order = 
    case value md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readHoldingRegisters (MB.TransactionId idx) 0 255 (register md) 1
            return md {value = ModWord resp}
        ModFloat _ -> do
            xs <- MB.readHoldingRegisters (MB.TransactionId idx) 0 255 reg_address 2
            case xs of
                [msw,lsw] -> return md {value = ModFloat $ getFloat order (msw,lsw)} 
                _ -> throwError $ MB.OtherException $ 
                            "Error reading Float from holding register address: "
                            ++ show reg_address
                            ++ ", "
                            ++ show (reg_address + 1)
  where 
    reg_address = register md

getFloat :: ByteOrder -> (Word16, Word16) -> Maybe Float
getFloat order ws
    | order == LE = Just $ le2float ws
    | order == BE = Just $ be2float ws 

le2float :: (Word16, Word16) -> Float
le2float (fst, snd)= runGet getFloatle $ runPut putWords 
  where
    putWords = putWord16le fst >> putWord16le snd

be2float :: (Word16, Word16) -> Float
be2float (fst, snd)= runGet getFloatbe $ runPut putWords 
  where
    putWords = putWord16le fst >> putWord16le snd

---------------------------------------------------
--- Explorarory stuff, nothing to see here!
---------------------------------------------------

testData :: [ModData]
testData =
  [ modData (T.pack "foo") InputRegister 1 (ModWord Nothing) (T.pack ""),
    modData (T.pack "bar") InputRegister 0 (ModWord Nothing) (T.pack ""),
    modData (T.pack "faz") InputRegister 2 (ModWord Nothing) (T.pack ""),
    modData (T.pack "pee") InputRegister 3 (ModWord Nothing) (T.pack "")
  ]

getStatus :: MB.Session [Word16]
getStatus = MB.readInputRegisters 0 0 0 0 5

getPower :: MB.Session [Word16]
getPower = MB.readInputRegisters 0 0 0 0 6

combined :: MB.Session [[Word16]]
combined = do
  a <- getStatus
  b <- getPower
  return [a, b]

local :: S.Socket -> MB.Connection
local s =
  MB.Connection
    { MB.connWrite = send s,
      MB.connRead = recv s,
      MB.connCommandTimeout = 1000,
      MB.connRetryWhen = \e n -> False
    }

addr = S.SockAddrInet 5502 (S.tupleToHostAddress (127, 0, 0, 1))

try :: S.SockAddr -> IO ()
try address = do
  putStrLn ("Connecting to " ++ show address ++ "...")
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s address
  putStrLn "connected"
  a <- runExceptT $ MB.runSession (local s) (modSession testData LE)
  print a
