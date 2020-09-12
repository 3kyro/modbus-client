{-# LANGUAGE ScopedTypeVariables #-}
module Modbus 
    (
      modSession
    , modUpdateSession
    , getFloats
    , fromFloats
    , word2Float
    , modbusConnection
    , withSocket
    , connect
    , maybeConnect
    , getAddr
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
import Network.Socket.ByteString (recv, send)

import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import Types
import Control.Exception.Safe (SomeException, try, bracket)
import Data.IP (toHostAddress, IPv4)
import qualified System.Timeout as TM

modbusConnection :: S.Socket -> Int -> MB.Connection
modbusConnection s tm =
    MB.Connection
    { MB.connWrite          = send s
    , MB.connRead           = recv s
    , MB.connCommandTimeout = tm * 1000
    , MB.connRetryWhen      = const . const False
    }


modSession :: [ModData] -> ByteOrder -> MB.Session [ModData]
modSession md order =
  let
    md' = zip [0 ..] md
  in
    mapM (`genReadSession` order) md'

modUpdateSession :: [ModDataUpdate] -> ByteOrder -> MB.Session [ModDataUpdate]
modUpdateSession md order =
  let
    md' = zip [0 ..] md
  in
    mapM (`genUpdateSession` order) md'

genReadSession :: (Word16, ModData) -> ByteOrder -> MB.Session ModData
genReadSession (idx, md) order =
  let
    tp = modRegType md
  in
    case tp of
        InputRegister -> readInputModData md idx order
        HoldingRegister -> readHoldingModData md idx order
        _ -> throwError $ MB.OtherException "Invalid function type requested"

genReadUpdateSession :: (Word16, ModDataUpdate) -> ByteOrder -> MB.Session ModDataUpdate
genReadUpdateSession (idx, mdu) order =
  let
    tp = modRegType $ mduModData mdu
  in
    case tp of
        InputRegister -> readInputModDataUpdate mdu idx order
        HoldingRegister -> readHoldingModDataUpdate mdu idx order
        _ -> throwError $ MB.OtherException "Invalid function type requested"

genWriteUpdateSession :: (Word16, ModDataUpdate) -> ByteOrder -> MB.Session ModDataUpdate
genWriteUpdateSession (idx, mdu) order =
  let
    tp = modRegType $ mduModData mdu
  in
    case tp of
        InputRegister -> throwError $ MB.OtherException "Invalid function type requested"
        HoldingRegister -> writeHoldingModDataUpdate mdu idx order
        _ -> throwError $ MB.OtherException "Invalid function type requested"

genUpdateSession :: (Word16, ModDataUpdate) -> ByteOrder -> MB.Session ModDataUpdate
genUpdateSession (idx, mdu) order =
    if not $ mduSelected mdu
    then return mdu
    else case mduRW mdu of
        MDURead -> genReadUpdateSession (idx, mdu) order
        MDUWrite -> genWriteUpdateSession (idx, mdu) order


readInputModData :: ModData -> Word16 -> ByteOrder -> MB.Session ModData
readInputModData md idx order =
    case modValue md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readInputRegisters (MB.TransactionId idx) 0 uid addr 1
            return md {modValue = ModWord resp}
        ModFloat _ -> do
            xs <- MB.readInputRegisters (MB.TransactionId idx) 0 uid addr 2
            case xs of
                [msw,lsw] -> return md {modValue = ModFloat $ Just $ word2Float order (msw,lsw)}
                _ -> throwError $ MB.OtherException $ 
                            "Error reading Float from input register addr: "
                            ++ show addr
                            ++ ", "
                            ++ show (addr + 1)
  where
    addr = MB.RegAddress $ modAddress md
    uid = MB.UnitId $ modUid md


readInputModDataUpdate :: ModDataUpdate -> Word16 -> ByteOrder -> MB.Session ModDataUpdate
readInputModDataUpdate mdu idx order =
    case modValue $ mduModData mdu of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readInputRegisters (MB.TransactionId idx) 0 uid addr 1
            return $ setMDUModValue mdu $ ModWord resp
        ModFloat _ -> do
            xs <- MB.readInputRegisters (MB.TransactionId idx) 0 uid addr 2
            case xs of
                [msw,lsw] -> return $ setMDUModValue mdu $ ModFloat $ Just $ word2Float order (msw,lsw)
                _ -> throwError $ MB.OtherException $ 
                            "Error reading Float from input register addr: "
                            ++ show addr
                            ++ ", "
                            ++ show (addr + 1)
  where
    addr = MB.RegAddress $ modAddress $ mduModData mdu
    uid = MB.UnitId $ modUid $ mduModData mdu
            
            
readHoldingModData :: ModData -> Word16 -> ByteOrder -> MB.Session ModData
readHoldingModData md idx order =
    case modValue md of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readHoldingRegisters (MB.TransactionId idx) 0 uid addr 1
            return md {modValue = ModWord resp}
        ModFloat _ -> do
            xs <- MB.readHoldingRegisters (MB.TransactionId idx) 0 uid addr 2
            case xs of
                [msw,lsw] -> return md {modValue = ModFloat $ Just $ word2Float order (msw,lsw)}
                _ -> throwError $ MB.OtherException $
                            "Error reading Float from holding register address: "
                            ++ show addr
                            ++ ", "
                            ++ show (addr + 1)
  where
    addr = MB.RegAddress $ modAddress md
    uid = MB.UnitId $ modUid md

readHoldingModDataUpdate :: ModDataUpdate -> Word16 -> ByteOrder -> MB.Session ModDataUpdate
readHoldingModDataUpdate mdu idx order =
    case modValue $ mduModData mdu of
        ModWord _ -> do
            resp <- listToMaybe <$> MB.readHoldingRegisters (MB.TransactionId idx) 0 uid addr 1
            return $ setMDUModValue mdu $ ModWord resp
        ModFloat _ -> do
            xs <- MB.readHoldingRegisters (MB.TransactionId idx) 0 uid addr 2
            case xs of
                [msw,lsw] -> return $ setMDUModValue mdu $ ModFloat $ Just $ word2Float order (msw,lsw)
                _ -> throwError $ MB.OtherException $
                            "Error reading Float from holding register address: "
                            ++ show addr
                            ++ ", "
                            ++ show (addr + 1)
  where
    addr = MB.RegAddress $ modAddress $ mduModData mdu
    uid = MB.UnitId $ modUid $ mduModData mdu

-- TODO : Byteorder!!!!!
writeHoldingModDataUpdate :: ModDataUpdate -> Word16 -> ByteOrder -> MB.Session ModDataUpdate
writeHoldingModDataUpdate mdu idx _ =
    case modValue $ mduModData mdu of
        ModWord (Just v) -> do
            MB.writeSingleRegister (MB.TransactionId idx) 0 uid addr v
            return mdu
        ModWord Nothing -> return mdu
        ModFloat (Just v) -> do
            MB.writeMultipleRegisters (MB.TransactionId idx) 0 uid addr [fst (float2Word v), snd (float2Word v)]
            return mdu
        ModFloat Nothing -> return mdu
  where
    addr = MB.RegAddress $ modAddress $ mduModData mdu
    uid = MB.UnitId $ modUid $ mduModData mdu


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
word2Float order ws@(f,s) =
    case order of
        LE -> le2float ws
        BE -> be2float ws
        LESW -> le2float (swappWord f, swappWord s)
        BESW -> be2float (swappWord f, swappWord s)

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

withSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withSocket addr = bracket (connect addr) close
  where close s = S.gracefulClose s 1000

connect :: S.SockAddr -> IO S.Socket
connect addr = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    return s

-- Connect to the server using a new socket and checking for a timeout
maybeConnect :: S.SockAddr -> Int -> IO (Maybe S.Socket)
maybeConnect addr tm = do
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    ( rlt :: Either SomeException (Maybe ()) ) <- try $ TM.timeout tm (S.connect s addr)
    case rlt of
        Left _ -> return Nothing
        Right m -> return $ s <$ m

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)