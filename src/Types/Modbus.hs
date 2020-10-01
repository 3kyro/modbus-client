{-# LANGUAGE FlexibleContexts #-}
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

    , MB.Config

    , Application
    , execApp

    , MBRegister
    , registerType
    , registerToWord16
    , registerFromWord16

    , Worker (..)

    , Session (..)

    , UID
    , setUID
    , getUID
    , TPU
    , setTPU
    , getTPU

    , RegType (..)

    , TCPClient
    , RTUClient

    ) where

import Control.Concurrent (putMVar, takeMVar, MVar)
import Control.Exception.Safe (throw, MonadThrow)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Tagged (Tagged, untag, Tagged(..))
import Data.Word (Word16, Word8)
import Data.Range (Range)
import qualified Network.Modbus.TCP as TCP
import qualified Network.Modbus.RTU as RTU
import qualified Network.Modbus.Protocol as MB
import Network.Modbus.Protocol (Address, Config)
import Test.QuickCheck (Arbitrary(..), elements, arbitrary)

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
    readMBRegister :: (MonadIO m, MonadThrow m, MBRegister b) => b -> TransactionInfo -> Range Address -> Tagged a (Session m (Maybe b))
    writeMBRegister :: (MonadIO m, MonadThrow m, MBRegister b, MonadThrow (Tagged TCPClient), MonadThrow (Tagged RTUClient)) => b -> TransactionInfo -> Address -> Tagged a (Session m ())

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

    readMBRegister reg tpu range =
        case registerType reg of
            InputRegister -> do
                rlt <- readInputRegisters tpu range
                Tagged $ registerFromWord16 reg <$> rlt
            HoldingRegister -> do
                rlt <- readHoldingRegisters tpu range
                Tagged $ registerFromWord16 reg <$> rlt

    writeMBRegister reg tpu address =
        case registerType reg of
            InputRegister -> throw $ MB.OtherException "Non writable Register Type"
            HoldingRegister -> do
                rlt <- writeMultipleRegisters tpu address $ registerToWord16 reg
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

    readMBRegister reg tpu range =
        case registerType reg of
            InputRegister -> do
                rlt <- readInputRegisters tpu range
                Tagged $ registerFromWord16 reg <$> rlt
            HoldingRegister -> do
                rlt <- readHoldingRegisters tpu range
                Tagged $ registerFromWord16 reg <$> rlt

    writeMBRegister reg tpu address =
        case registerType reg of
            InputRegister -> throw $ MB.OtherException "Non writable Register Type"
            HoldingRegister -> do
                rlt <- writeMultipleRegisters tpu address $ registerToWord16 reg
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
-- MBRegister
---------------------------------------------------------------------------------------------------------------

-- A type that can be converted to a modbus register
class MBRegister a where
    registerType :: a -> RegType
    registerToWord16 :: a -> [Word16]
    registerFromWord16 :: a -> [Word16] -> Maybe a

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