{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings       #-}

{-# LANGUAGE ScopedTypeVariables     #-}


module Types.Modbus
    ( ModbusClient
    , runClient
    , readInputRegisters
    , readHoldingRegisters
    , writeSingleRegister
    , writeMultipleRegisters
    , readMBRegister
    , writeMBRegister

    , Config
    , Address (..)

    , Application
    , execApp

    , MBRegister (..)

    , Worker (..)

    , Session (..)

    , TransactionInfo (..)
    , UID
    , getUID
    , TPU
    , setTPU
    , TID
    , initTID
    , getNewTID


    , RegType (..)
    , serializeRegType

    , Client (..)
    , ModbusProtocol (..)

    , ByteOrder (..)
    , float2Word16
    , word16ToFloat

    , HeartBeat (..)
    , heartBeatSignal

    , keepAliveThread

    , getTCPConfig
    , getRTUConfig
    , getAddr
    , maybeTCPConnect

    ) where

import           Control.Concurrent         (MVar, ThreadId, forkFinally,
                                             newEmptyMVar, putMVar, takeMVar,
                                             threadDelay)
import           Control.Exception.Safe     (MonadMask, MonadThrow,
                                             SomeException, bracket, throw, try)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..))
import           Data.Binary.Get            (getFloatbe, getFloatle,
                                             getWord16host, runGet)
import           Data.Binary.Put            (putFloatbe, putFloatle,
                                             putWord16host, runPut)
import           Data.IP                    (toHostAddress)
import           Data.IP.Internal           (IPv4)
import           Data.Range                 (begin, Range)
import           Data.Word                  (Word16, Word8)
import           Network.Modbus.Protocol    (Address, Config)
import           Test.QuickCheck            (Arbitrary (..), arbitrary,
                                             elements)


import qualified Data.ByteString.Char8      as B
import qualified Network.Modbus.Protocol    as MB
import qualified Network.Modbus.RTU         as RTU
import qualified Network.Modbus.TCP         as TCP
import qualified Network.Socket             as S
import qualified Network.Socket.ByteString  as S

import           Control.Concurrent.STM     (TVar, atomically, modifyTVar',
                                             newTVarIO, readTVarIO)
import qualified System.Hardware.Serialport as SP
import qualified System.Timeout             as TM
import Data.Data (Proxy)


---------------------------------------------------------------------------------------------------------------
-- ModbusClient
---------------------------------------------------------------------------------------------------------------

class ModbusClient a where
    runClient :: (MonadIO m, MonadThrow m, Application m, MonadMask m)
        => Worker m     -- Aworker that will execute the session
        -> MVar a       -- The Client used by all threads
        -> Session m b  -- Session to be run
        -> m b
    readInputRegisters :: (MonadIO m, MonadThrow m)
        => ModbusProtocol
        -> TransactionInfo
        -> Range Address
        -> Session m [Word16]
    readHoldingRegisters :: (MonadIO m, MonadThrow m)
        => ModbusProtocol
        -> TransactionInfo
        -> Range Address
        -> Session m [Word16]
    writeSingleRegister :: (MonadIO m, MonadThrow m)
        => ModbusProtocol
        -> TransactionInfo
        -> Address
        -> Word16
        -> Session m ()
    writeMultipleRegisters :: (MonadIO m, MonadThrow m)
        => ModbusProtocol
        -> TransactionInfo
        -> Address
        -> [Word16]
        -> Session m ()
    readMBRegister :: (MonadIO m, MonadThrow m, MBRegister b)
        => Proxy a
        -> ModbusProtocol
        -> TransactionInfo
        -> ByteOrder
        -> b
        -> Session m (Maybe b)
    writeMBRegister :: (MonadIO m, MonadThrow m, MBRegister b)
        => Proxy a
        -> ModbusProtocol
        -> TransactionInfo
        -> ByteOrder
        -> b
        -> Session m ()

---------------------------------------------------------------------------------------------------------------
-- ModbusProtocol
---------------------------------------------------------------------------------------------------------------

data ModbusProtocol = ModBusTCP
    | ModBusRTU

---------------------------------------------------------------------------------------------------------------
-- Client
---------------------------------------------------------------------------------------------------------------

newtype Client = Client Config

instance ModbusClient Client where

    runClient worker configMVar session =
        bracket getClient releaseClient
            $ \(Client config) ->
                    case worker of
                            TCPWorker tcpworker -> do
                                let TCPSession session' = session
                                TCP.runSession tcpworker config session'
                            RTUWorker rtuworker -> do
                                let RTUSession session' = session
                                RTU.runSession rtuworker config session'
      where
        getClient = liftIO $ takeMVar configMVar
        releaseClient = liftIO . putMVar configMVar

    readInputRegisters ModBusTCP tpu range = TCPSession (TCP.readInputRegisters (getTPU tpu) range)
    readInputRegisters ModBusRTU tpu range = RTUSession (RTU.readInputRegisters (RTU.UnitId $ getUID tpu) range)

    readHoldingRegisters ModBusTCP tpu range = TCPSession (TCP.readHoldingRegisters (getTPU tpu) range)
    readHoldingRegisters ModBusRTU tpu range = RTUSession (RTU.readHoldingRegisters (RTU.UnitId $ getUID tpu) range)

    writeSingleRegister ModBusTCP tpu address value = TCPSession (TCP.writeSingleRegister (getTPU tpu) address value)
    writeSingleRegister ModBusRTU tpu address value = RTUSession (RTU.writeSingleRegister (RTU.UnitId $ getUID tpu) address value)

    writeMultipleRegisters ModBusTCP tpu address values = TCPSession (TCP.writeMultipleRegisters (getTPU tpu) address values)
    writeMultipleRegisters ModBusRTU tpu address values = RTUSession (RTU.writeMultipleRegisters (RTU.UnitId $ getUID tpu) address values)

    readMBRegister _ protocol tpu bo reg =
        case registerType reg of
            InputRegister -> do
                let rlt = readInput
                registerFromWord16 bo reg <$> rlt
            HoldingRegister -> do
                let rlt = readHolding
                registerFromWord16 bo reg <$> rlt
      where
        readInput = case protocol of
            ModBusTCP -> TCPSession (TCP.readInputRegisters (getTPU tpu) range)
            ModBusRTU -> RTUSession (RTU.readInputRegisters (RTU.UnitId $ getUID tpu) range)
        readHolding = case protocol of
            ModBusTCP -> TCPSession (TCP.readHoldingRegisters (getTPU tpu) range)
            ModBusRTU -> RTUSession (RTU.readHoldingRegisters (RTU.UnitId $ getUID tpu) range)
        range = registerAddress reg

    writeMBRegister _ protocol tpu bo reg =
        case protocol of
            ModBusTCP ->
                TCPSession $ case registerType reg of
                    InputRegister -> throw $ MB.OtherException "Non writable Register Type"
                    HoldingRegister -> TCP.writeMultipleRegisters (getTPU tpu) address values
            ModBusRTU ->
                RTUSession $ case registerType reg of
                    InputRegister -> throw $ MB.OtherException "Non writable Register Type"
                    HoldingRegister -> RTU.writeMultipleRegisters (RTU.UnitId $ getUID tpu) address values
      where
        values = registerToWord16 bo reg
        address = begin $ registerAddress reg

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
    registerAddress :: a -> Range Address
    registerToWord16 :: ByteOrder -> a -> [Word16]
    registerFromWord16 :: ByteOrder -> a -> [Word16] -> Maybe a

---------------------------------------------------------------------------------------------------------------
-- Worker
---------------------------------------------------------------------------------------------------------------

-- Workers execute Sessions
data Worker m = RTUWorker (RTU.Worker m)
    | TCPWorker (TCP.Worker m)


---------------------------------------------------------------------------------------------------------------
-- Session
---------------------------------------------------------------------------------------------------------------

-- AN RTU/TCP Session
-- Sessions are executed by workers and can be batched
data Session m a = RTUSession (RTU.Session m a)
    | TCPSession (TCP.Session m a)
    deriving (Functor)

---------------------------------------------------------------------------------------------------------------
-- TransactionInfo
---------------------------------------------------------------------------------------------------------------

-- A server is mainly defined by a unit id
-- In case the Modbus TCP protocol is used, every transaction
-- must have its unique id. This is not necessary for Modbus RTU
data TransactionInfo = TransactionInfo
    { unitId        :: Word8
    , transactionId :: TID
    }

newtype TID = TID {unTID :: Word16}

initTID :: IO (TVar TID)
initTID = newTVarIO $ TID 0

-- Increments the transaction Id of the TPU by one
getNewTID :: TVar TID -> IO TID
getNewTID tid = atomically (modifyTVar' tid incr) >> readTVarIO tid
  where
    incr = TID . (+ 1) . unTID

-- Exportable types
type UID = TransactionInfo
type TPU = TransactionInfo

setTPU :: Word8 -> TID -> TPU
setTPU = TransactionInfo

getUID :: UID -> Word8
getUID = unitId

getTPU :: TPU -> TCP.TPU
getTPU (TransactionInfo uid tid) = TCP.TPU
    (TCP.TransactionId $ unTID tid)
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
data RegType = DiscreteInput
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
            DiscreteInput   -> String "discrete input"
            Coil            -> String "coil"
            InputRegister   -> String "input register"
            HoldingRegister -> String "holding register"

instance FromJSON RegType where
    parseJSON (String s) =
        case s of
            "dicrete input"    -> return DiscreteInput
            "coil"             -> return Coil
            "input register"   -> return InputRegister
            "holding register" -> return HoldingRegister
            _                  -> fail "Not a RegType"
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
data ByteOrder = LE
    | BE
    deriving (Show, Read, Eq)

instance Arbitrary ByteOrder where
    arbitrary = elements [LE, BE]

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

-- A HeartBeat signal
data HeartBeat = HeartBeat
    { hbAddress  :: !MB.Address -- Address
    -- Interval
    , hbInterval :: !Int -- Interval
    -- ThreadId
    , hbThreadId :: !ThreadId -- ThreadId
    -- Status : Empty = Ok , SomeException = thread has panicked
    , hbStatus   :: MVar SomeException -- Status : Empty = Ok , SomeException = thread has panicked
    } deriving (Eq)

-- Spawns a heartbeat signal thread
heartBeatSignal :: (MonadIO m, Application m, ModbusClient a, MonadThrow m, MonadMask m)
    => ModbusProtocol
    -> Int              -- HeartBeat signal period in ms
    -> Worker m         -- Worker to execute the session
    -> MVar a           -- Client configuration
    -> Word8
    -> TVar TID
    -> Address          -- HeartBeat signal register address
    -> m HeartBeat
heartBeatSignal protocol interval worker clientMVar uid tid address = do
    status <- liftIO newEmptyMVar
    threadid <- liftIO $ forkFinally (execApp $ thread address 0) (finally status)
    return $ HeartBeat address interval threadid status
  where
    thread address' acc = do
        liftIO $ threadDelay interval
        tid' <- liftIO $ getNewTID tid
        let tpu' = setTPU uid tid'
        let session = case protocol of
                ModBusTCP -> TCPSession (TCP.writeSingleRegister (getTPU tpu') address acc)
                ModBusRTU -> RTUSession (RTU.writeSingleRegister (RTU.UnitId uid) address acc)
        runClient worker clientMVar session
        thread address' (acc + 1)
    finally status terminationStatus =
        case terminationStatus of
            Left someExcept ->
                putMVar status someExcept
            Right ()        ->
                return ()

---------------------------------------------------------------------------------------------------------------
-- Keep Alive
---------------------------------------------------------------------------------------------------------------

-- A thread implementing the TCP keep alive function
-- The global ThreadConfig contains information on wheter keep alive is active
-- and the interval between calls. In order be cross platform, we ignore OS specific
-- implementation of the keep alive function and simply send a minimum package at every interval
keepAliveThread :: MVar Client -> Int -> IO ()
keepAliveThread clientMVar interval = do
    threadDelay interval
    -- take the global TCP client
    bracket (liftIO $ takeMVar clientMVar) (liftIO . putMVar clientMVar)
        $ \(Client config) ->
            MB.cfgWrite config $ B.pack ""
    -- recurse
    keepAliveThread clientMVar interval

---------------------------------------------------------------------------------------------------------------
-- Config
---------------------------------------------------------------------------------------------------------------

getTCPConfig :: S.Socket -> Int -> Config
getTCPConfig s timeout = MB.Config
    { MB.cfgWrite = S.send s
    , MB.cfgRead = S.recv s 4096
    , MB.cfgCommandTimeout = timeout
    , MB.cfgRetryWhen = const . const False
    , MB.cfgEnableBroadcasts = False
    }

getRTUConfig :: SP.SerialPort -> Int -> Config
getRTUConfig s timeout = MB.Config
    { MB.cfgWrite = SP.send s
    , MB.cfgRead = SP.recv s 4096
    , MB.cfgCommandTimeout = timeout
    , MB.cfgRetryWhen = const . const False
    , MB.cfgEnableBroadcasts = True
    }

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)

-- Connect to the server using a new socket and checking for a timeout
maybeTCPConnect :: S.SockAddr -> Int -> IO (Maybe S.Socket)
maybeTCPConnect addr tm = do
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    ( rlt :: Either SomeException (Maybe ()) ) <- try $ TM.timeout tm (S.connect s addr)
    case rlt of
        Left _  -> return Nothing
        Right m -> return $ s <$ m
