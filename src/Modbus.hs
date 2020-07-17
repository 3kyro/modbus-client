module Modbus (modSession) where

import Control.Monad.Except (throwError)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Data.Binary.Put (runPut, putWord16le)
import Data.Binary.Get (runGet, getFloatbe, getFloatle)

import CsvParser (ModData (..), ModType (..), RegType (..), ByteOrder (..))

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
le2float (f, s)= runGet getFloatle $ runPut putWords 
  where
    putWords = putWord16le f >> putWord16le s

be2float :: (Word16, Word16) -> Float
be2float (f, s)= runGet getFloatbe $ runPut putWords 
  where
    putWords = putWord16le f >> putWord16le s