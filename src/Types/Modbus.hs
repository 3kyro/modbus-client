{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Types.Modbus
    ( Client
    , runClient
    , readInputRegisters
    , readHoldingRegisters
    , writeSingleRegister
    , writeMultipleRegisters
    , readMBRegister
    , writeMBRegister

    , Config
    , Address

    , Application
    , execApp
    , AppConfig (..)

    , MBRegister
    , registerType
    , registerToWord16
    , registerFromWord16

    , Worker (..)

    , Session (..)

    , TransactionInfo
    , UID
    , setUID
    , getUID
    , TPU
    , setTPU
    , getTPU
    , incrTID

    , RegType (..)
    , serializeRegType

    , TCPClient
    , RTUClient

    , ByteOrder (..)
    , float2Word16
    , word16ToFloat

    , heartBeatSignal

    , keepAliveThread
    ) where

import Control.Concurrent (threadDelay, forkIO, ThreadId, putMVar, takeMVar, MVar)
import Control.Exception.Safe (throw, MonadThrow)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Aeson (Value (..), FromJSON (..), ToJSON (..))
import Data.Binary.Get
    ( runGet
    , getFloatbe
    , getFloatle
    , getWord16host
    )
import Data.Binary.Put
    ( runPut
    , putFloatbe
    , putFloatle
    , putWord16host

    )
import Data.Tagged (Tagged, untag, Tagged(..))
import Data.Word (Word16, Word8)
import Data.Range (Range)
import Network.Modbus.Protocol (Address, Config)
import Test.QuickCheck (Arbitrary(..), elements, arbitrary)

import qualified Data.ByteString.Char8 as B
import qualified Network.Modbus.TCP as TCP
import qualified Network.Modbus.RTU as RTU
import qualified Network.Modbus.Protocol as MB

---------------------------------------------------------------------------------------------------------------
-- Client
---------------------------------------------------------------------------------------------------------------

class Client a where
    runClient :: (MonadIO m, MonadThrow m, Application m)
        => Worker m -- Worker for the session
        -> MVar a -- The Client used by all threads
        -> Tagged a (Session m b) -- Session to be run
        -> m b
    readInputRegisters :: (MonadIO m, MonadThrow m) => TransactionInfo -> Range Address -> Tagged a (Session m [Word16])
    readHoldingRegisters :: (MonadIO m, MonadThrow m) => TransactionInfo -> Range Address -> Tagged a (Session m [Word16])
    writeSingleRegister :: (MonadIO m, MonadThrow m) => TransactionInfo -> Address -> Word16 -> Tagged a (Session m ())
    writeMultipleRegisters :: (MonadIO m, MonadThrow m) => TransactionInfo -> Address -> [Word16] -> Tagged a (Session m ())
    readMBRegister :: (MonadIO m, MonadThrow m, MBRegister b) => b -> TransactionInfo -> Range Address -> ByteOrder -> Tagged a (Session m (Maybe b))
    writeMBRegister :: (MonadIO m, MonadThrow m, MBRegister b, MonadThrow (Tagged TCPClient), MonadThrow (Tagged RTUClient)) => b -> TransactionInfo -> Address -> ByteOrder -> Tagged a (Session m ())

---------------------------------------------------------------------------------------------------------------
-- TCP Client
---------------------------------------------------------------------------------------------------------------

newtype TCPClient = TCPClient Config

instance Client TCPClient where

    runClient (TCPWorker worker) configMVar session = do
        -- get the global client
        (TCPClient config) <- liftIO $ takeMVar configMVar
        rlt <- TCP.runSession worker config session'
        -- release the global client
        liftIO $ putMVar configMVar (TCPClient config)
        return rlt
        where TCPSession session'= untag session

    readInputRegisters tpu range = Tagged $ TCPSession (TCP.readInputRegisters (getTPU tpu) range)

    readHoldingRegisters tpu range = Tagged $ TCPSession (TCP.readHoldingRegisters (getTPU tpu) range)

    writeSingleRegister tpu address value = Tagged $ TCPSession (TCP.writeSingleRegister (getTPU tpu) address value)

    writeMultipleRegisters tpu address values = Tagged $ TCPSession (TCP.writeMultipleRegisters (getTPU tpu) address values)

    readMBRegister reg tpu range bo =
        case registerType reg of
            InputRegister -> do
                rlt <- readInputRegisters tpu range
                Tagged $ registerFromWord16 bo reg <$> rlt
            HoldingRegister -> do
                rlt <- readHoldingRegisters tpu range
                Tagged $ registerFromWord16 bo reg <$> rlt

    writeMBRegister reg tpu address bo =
        case registerType reg of
            InputRegister -> throw $ MB.OtherException "Non writable Register Type"
            HoldingRegister -> do
                rlt <- writeMultipleRegisters tpu address $ registerToWord16 bo reg
                Tagged rlt

---------------------------------------------------------------------------------------------------------------
-- RTU Client
---------------------------------------------------------------------------------------------------------------

newtype RTUClient = RTUClient Config

instance Client RTUClient where
    runClient (RTUWorker worker) configMVar session = do
        -- get the global client
        (RTUClient config) <- liftIO $ takeMVar configMVar
        rlt <- RTU.runSession worker config session'
        -- release the global client
        liftIO $ putMVar configMVar (RTUClient config)
        return rlt
        where RTUSession session'= untag session

    readInputRegisters tpu range = Tagged $ RTUSession (RTU.readInputRegisters (RTU.UnitId $ getUID tpu) range)

    readHoldingRegisters tpu range = Tagged $ RTUSession (RTU.readHoldingRegisters (RTU.UnitId $ getUID tpu) range)

    writeSingleRegister tpu address value = Tagged $ RTUSession (RTU.writeSingleRegister (RTU.UnitId $ getUID tpu) address value)

    writeMultipleRegisters tpu address values = Tagged $ RTUSession (RTU.writeMultipleRegisters (RTU.UnitId $ getUID tpu) address values)

    readMBRegister reg tpu range bo =
        case registerType reg of
            InputRegister -> do
                rlt <- readInputRegisters tpu range
                Tagged $ registerFromWord16 bo reg <$> rlt
            HoldingRegister -> do
                rlt <- readHoldingRegisters tpu range
                Tagged $ registerFromWord16 bo reg <$> rlt

    writeMBRegister reg tpu address bo =
        case registerType reg of
            InputRegister -> throw $ MB.OtherException "Non writable Register Type"
            HoldingRegister -> do
                rlt <- writeMultipleRegisters tpu address $ registerToWord16 bo reg
                Tagged rlt

---------------------------------------------------------------------------------------------------------------
-- Application
---------------------------------------------------------------------------------------------------------------

-- A monad that can be executed
class Application m where
    execApp :: m a -> IO a


instance Application IO where
    execApp action = action

---------------------------------------------------------------------------------------------------------------
-- Application Configuration
---------------------------------------------------------------------------------------------------------------

-- Configuration for a modbus application
data AppConfig = AppConfig
    { keepAliveFlag :: !Bool        -- Keep alive enabled/disabled flag
    , keepAliveTime :: !Int         -- Keep alive intrval
    , hearbeatPool  :: ![ThreadId]  -- Pool of heartbeat signal threads
    }

---------------------------------------------------------------------------------------------------------------
-- MBRegister
---------------------------------------------------------------------------------------------------------------

-- A type that can be converted to a modbus register
class MBRegister a where
    registerType :: a -> RegType
    registerToWord16 :: ByteOrder -> a -> [Word16]
    registerFromWord16 :: ByteOrder -> a -> [Word16] -> Maybe a

---------------------------------------------------------------------------------------------------------------
-- Worker
---------------------------------------------------------------------------------------------------------------

-- Workers execute Sessions
data Worker m = RTUWorker (RTU.Worker m) | TCPWorker (TCP.Worker m)

---------------------------------------------------------------------------------------------------------------
-- Session
---------------------------------------------------------------------------------------------------------------

-- AN RTU/TCP Session
-- Sessions are executed by workers and can be batched
data Session m a
    = RTUSession (RTU.Session m a) | TCPSession (TCP.Session m a)
    deriving (Functor)

---------------------------------------------------------------------------------------------------------------
-- TransactionInfo
---------------------------------------------------------------------------------------------------------------

-- A server is mainly defined by a unit id
-- In case the Modbus TCP protocol is used, every transaction
-- must have its unique id. This is not necessary for Modbus RTU
data TransactionInfo = TransactionInfo
    { unitId :: Word8
    , transactionId :: Word16
    }

-- Exportable types
type UID = TransactionInfo
type TPU = TransactionInfo

setUID :: Word8 -> UID
setUID uid = TransactionInfo uid 0

setTPU :: Word8 -> Word16 -> TPU
setTPU = TransactionInfo

getUID :: UID -> Word8
getUID = unitId

getTPU :: TPU -> TCP.TPU
getTPU (TransactionInfo uid tid) = TCP.TPU
    (TCP.TransactionId tid)
    TCP.ModbusTcp
    (TCP.UnitId uid)

-- Increments the transaction Id of the TPU by one
incrTID :: TPU -> TPU
incrTID tpu = tpu { transactionId = transactionId tpu + 1}

---------------------------------------------------------------------------------------------------------------
-- RegType
---------------------------------------------------------------------------------------------------------------

-- Modbus Register type:
-- Discrete Input, single bit, read only
-- Coil Single bit, read / write
-- Input Register, 16-bit word, read only
-- Holding Register, 16-bit word, read / write
data RegType
    = DiscreteInput
    | Coil
    | InputRegister
    | HoldingRegister
    deriving (Eq)

instance Show RegType where
    show DiscreteInput   = "Discrete Input"
    show Coil            = "Coil"
    show InputRegister   = "Input Register"
    show HoldingRegister = "Holding Register"

instance Arbitrary RegType where
  arbitrary = elements [DiscreteInput, Coil, InputRegister, HoldingRegister]

instance ToJSON RegType where
    toJSON rt =
        case rt of
            DiscreteInput -> String "discrete input"
            Coil -> String "coil"
            InputRegister -> String "input register"
            HoldingRegister -> String "holding register"

instance FromJSON RegType where
    parseJSON (String s) =
        case s of
            "dicrete input" -> return DiscreteInput
            "coil" -> return Coil
            "input register" -> return InputRegister
            "holding register" -> return HoldingRegister
            _ -> fail "Not a RegType"
    parseJSON _ = fail "Not a RegType"

serializeRegType :: RegType -> String
serializeRegType rt =
    case rt of
        DiscreteInput   -> "discrete input"
        Coil            -> "coil"
        InputRegister   -> "input register"
        HoldingRegister -> "holding register"

---------------------------------------------------------------------------------------------------------------
-- ByteOrder
---------------------------------------------------------------------------------------------------------------
-- Modbus uses a 'big-endian' encoding for all transmitted values by default.
-- In order to encode data values bigger than a 16-bit word, multiple 16 bit registers have to
-- be used.
-- Byte order defines the encoding used by the client for these multiple word data types
-- Eg: when receiving two two-byte words AB and CD
-- LE   - AB CD
-- BE   - CD AB
data ByteOrder
    = LE    -- Little Endian
    | BE    -- Big Endian
    deriving (Show, Read, Eq)


-- Converts a Float to a list of Word16s
float2Word16 :: ByteOrder -> Float -> [Word16]
float2Word16 LE float =
    runGet (combine <$> getWord16host <*> getWord16host) floatle
  where
    floatle = runPut $ putFloatle float
    combine a b = a : [b]
float2Word16 BE float =
    runGet (combine <$> getWord16host <*> getWord16host) floatbe
  where
    floatbe = runPut $ putFloatbe float
    combine a b = a : [b]

-- Converts a list of Word16s to a Maybe Float
word16ToFloat :: ByteOrder -> [Word16] -> Maybe Float
word16ToFloat _ [] = Nothing
word16ToFloat LE [a,b] =
    Just $ runGet getFloatle float
  where
    float = runPut $ putWord16host a  >> putWord16host b
word16ToFloat BE [a,b] =
    Just $ runGet getFloatbe float
  where
    float = runPut $ putWord16host a  >> putWord16host b
word16ToFloat _ _ = Nothing

---------------------------------------------------------------------------------------------------------------
-- HeartBeat Signal
---------------------------------------------------------------------------------------------------------------

-- Spawns a heartbeat signal thread
heartBeatSignal :: (MonadIO m, Application m, Client a, MonadThrow m)
    => Int              -- Heartbeat signal period in ms
    -> Worker m         -- Worker to execute the session
    -> MVar a           -- Client configuration
    -> TransactionInfo  -- Session's transaction info
    -> Address          -- Heartbeat signal register address
    -> m ThreadId
heartBeatSignal timer worker clientMVar tpu address =
    liftIO $ forkIO $ execApp $ thread address 0
    where
    thread address' acc = do
        liftIO $ threadDelay timer
        let session = writeSingleRegister tpu address' acc
        runClient worker clientMVar session
        thread address' (acc + 1)
        return ()

---------------------------------------------------------------------------------------------------------------
-- Keep Alive
---------------------------------------------------------------------------------------------------------------

-- A thread implementing the TCP keep alive function
-- The global ThreadConfig contains information on wheter keep alive is active
-- and the interval between calls. In order be cross platform, we ignore OS specific
-- implementation of the keep alive function and simply send a minimum package at every interval
keepAliveThread :: MVar AppConfig -> MVar TCPClient -> IO ()
keepAliveThread threadConfigMVar clientMVar = do
    -- Check if keep alive is active
    threadConfig <- takeMVar threadConfigMVar
    if keepAliveFlag threadConfig
    then do
        -- release threadConfig
        putMVar threadConfigMVar threadConfig
        go threadConfigMVar clientMVar (keepAliveTime threadConfig)
    else do
        -- release threadConfig
        putMVar threadConfigMVar threadConfig
        return ()
  where
      go threadConfigMvar configMvar timer = do
        threadDelay timer
        -- take the global TCP client
        TCPClient config <- takeMVar configMvar
        let write = MB.cfgWrite config
        write $ B.pack ""
        -- release the global TCP client
        putMVar clientMVar $ TCPClient config
        -- recurse
        keepAliveThread threadConfigMvar configMvar
