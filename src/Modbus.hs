module Modbus 
    (
      modSession
    , Config (..)
    , getFloats
    , fromFloats
    , word2Float
    ) where

import Control.Monad.Except (throwError)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Data.Binary.Put 
    (
       runPut
     , putWord16le
     , putFloatle
     )
import Data.Binary.Get 
    (
      runGet
    , getFloatbe
    , getFloatle
    , getWord16be
    , getWord16le
    )

import CsvParser (ModData (..), ModType (..), RegType (..), ByteOrder (..))

import qualified System.Modbus.TCP as MB

data Config = Config {
      conn :: MB.Connection
    , ord :: ByteOrder
    , modData :: [ModData]
}

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
            resp <- listToMaybe <$> MB.readInputRegisters (MB.TransactionId idx) 0 255 addr 1
            return md {value = ModWord resp}
        ModFloat _ -> do
            xs <- MB.readInputRegisters (MB.TransactionId idx) 0 255 addr 2
            case xs of
                [msw,lsw] -> return md {value = ModFloat $ Just $ word2Float order (msw,lsw)} 
                _ -> throwError $ MB.OtherException $ 
                            "Error reading Float from input register addr: "
                            ++ show addr
                            ++ ", "
                            ++ show (addr + 1)
  where 
    addr = MB.RegAddress $ register md
            
            
readHoldingModData :: ModData -> Word16 -> ByteOrder -> MB.Session ModData
readHoldingModData md idx order = 
    case value md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readHoldingRegisters (MB.TransactionId idx) 0 255 addr 1
            return md {value = ModWord resp}
        ModFloat _ -> do
            xs <- MB.readHoldingRegisters (MB.TransactionId idx) 0 255 addr 2
            case xs of
                [msw,lsw] -> return md {value = ModFloat $ Just $ word2Float order (msw,lsw)} 
                _ -> throwError $ MB.OtherException $ 
                            "Error reading Float from holding register address: "
                            ++ show addr
                            ++ ", "
                            ++ show (addr + 1)
  where 
    addr = MB.RegAddress $ register md

-- Converts a list of words to a list of floats 
getFloats :: ByteOrder -> [Word16] -> [Float]
getFloats _ []  = []
getFloats _ [_] = []
getFloats bo (x:y:ys) = word2Float bo (x,y) : getFloats bo ys

-- Converts a list of floats to a list of words
-- uses the default modbus protocol big-endian byte order
fromFloats :: [Float] -> [Word16]
fromFloats [] = []
fromFloats (x:xs) = 
  let
      (msw,lsw) = float2Word x
  in
      [msw,lsw] ++ fromFloats xs  

word2Float :: ByteOrder -> (Word16, Word16) -> Float
word2Float order ws@(f,s) 
    | order == LE = le2float ws
    | order == BE = be2float ws
    | order == LESW = le2float (swappWord f, swappWord s)
    | order == BESW = be2float (swappWord f, swappWord s)

float2Word :: Float -> (Word16, Word16)
float2Word fl = runGet getWords $ runPut $ putFloatle fl
  where
      getWords = (,) <$> getWord16le <*> getWord16le

le2float :: (Word16, Word16) -> Float
le2float (f, s)= runGet getFloatle $ runPut putWords 
  where
    putWords = putWord16le f >> putWord16le s

be2float :: (Word16, Word16) -> Float
be2float (f, s)= runGet getFloatbe $ runPut putWords 
  where
    putWords = putWord16le f >> putWord16le s

-- Swaps the two bytes of the provided Word16
swappWord :: Word16 -> Word16
swappWord w = runGet getWord16be $ runPut $ putWord16le w

