module Modbus where

import Control.Monad.Except (runExceptT, throwError)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Network.Socket.ByteString (recv, send)

import CsvParser (ModData (..), ModType (..), RegType (..), modData)

import qualified Data.Text as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

modSession :: [ModData] -> MB.Session [ModData]
modSession md =
  let 
    md' = zip [0 ..] md
  in 
    mapM genSession md'
  
genSession :: (Word16, ModData) -> MB.Session ModData
genSession (idx, md) =
  let 
    addr = MB.unRegAddress $ register md
    tp = regType md
  in 
    case tp of
        InputRegister -> readInputModData md idx
        HoldingRegister -> readHoldingModData md idx
        _ -> throwError $ MB.OtherException "Invalid function type requested"

readInputModData :: ModData -> Word16 -> MB.Session ModData
readInputModData md idx = 
    case value md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readInputRegisters (MB.TransactionId idx) 0 255 (register md) 1
            return md {value = ModWord resp}

readHoldingModData :: ModData -> Word16 -> MB.Session ModData
readHoldingModData md idx = 
    case value md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readHoldingRegisters (MB.TransactionId idx) 0 255 (register md) 1
            return md {value = ModWord resp}

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
  a <- runExceptT $ MB.runSession (local s) (mapM genSession (zip [0 ..] testData))
  print a
