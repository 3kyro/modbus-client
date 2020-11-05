{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modbus
    ( ModbusClient (..)
    , Config (..)
    , Address (..)
    , Application
    , execApp
    , MBRegister (..)
    , TCPWorker (..)
    , RTUWorker (..)
    , tcpDirectWorker
    , tcpBatchWorker
    , rtuDirectWorker
    , rtuBatchWorker
    , TCPSession (..)
    , RTUSession (..)
    , TransactionInfo (..)
    , UID
    , getUID
    , TPU
    , setTPU
    , TID (..)
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
    , getTCPClient
    , getRTUClient
    , getTCPConfig
    , getRTUConfig
    , getAddr
    , getTCPSocket
    , getRTUSerialPort
    , tcpReadInputRegisters
    , rtuReadInputRegisters
    , tcpReadHoldingRegisters
    , rtuReadHoldingRegisters
    , tcpWriteSingleRegister
    , rtuWriteSingleRegister
    , tcpWriteMultipleRegisters
    , rtuWriteMultipleRegisters
    , tcpReadMBRegister
    , rtuReadMBRegister
    , tcpWriteMBRegister
    , rtuWriteMBRegister
    , tcpUpdateMBRegister
    , rtuUpdateMBRegister
    , SerialSettings (..)
    , buildSerialSettings
    , BaudRate (..)
    , StopBits (..)
    , Parity (..)
    , newHeartBeat
, ) where

import Control.Concurrent (
    MVar,
    ThreadId,
    forkFinally,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    threadDelay,
 )
import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
 )
import Control.Exception.Safe (
    MonadMask,
    MonadThrow,
    SomeException,
    bracket,
    throw,
    try,
 )
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    (.:),
    (.=),
 )
import Data.Binary.Get (getFloatle, getWord16host, runGet)
import Data.Binary.Put (putFloatle, putWord16host, runPut)
import Data.IP (toHostAddress)
import Data.IP.Internal (IPv4)
import Data.Range (Range (..), begin)
import Data.Word (Word16, Word8)
import Network.Modbus.Protocol (Address, Config)
import Test.QuickCheck (
    Arbitrary (..),
    arbitrary,
    elements,
 )

import qualified Network.Modbus.Protocol as MB
import qualified Network.Modbus.RTU as RTU
import qualified Network.Modbus.TCP as TCP
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import qualified System.Hardware.Serialport as SP
import qualified System.Timeout as TM

---------------------------------------------------------------------------------------------------------------
-- ModbusClient
---------------------------------------------------------------------------------------------------------------

class ModbusClient a where
    tcpRunClient ::
        (MonadIO m, MonadThrow m, Application m, MonadMask m) =>
        TCPWorker m -> -- A worker that will execute the session
        MVar a -> -- The Client used by all threads
        TCPSession m b -> -- TCPSession to be run
        m b
    rtuRunClient ::
        (MonadIO m, MonadThrow m, Application m, MonadMask m) =>
        RTUWorker m -> -- A worker that will execute the session
        MVar a -> -- The Client used by all threads
        RTUSession m b -> -- RTUSession to be run
        m b

---------------------------------------------------------------------------------------------------------------
-- ModbusProtocol
---------------------------------------------------------------------------------------------------------------

data ModbusProtocol
    = ModBusTCP
    | ModBusRTU

instance FromJSON ModbusProtocol where
    parseJSON (String s) =
        case s of
            "TCP" -> return ModBusTCP
            "RTU" -> return ModBusRTU
            _ -> fail "Not a ModBusProtocol"
    parseJSON _ = fail "Not a ModBusProtocol"

instance ToJSON ModbusProtocol where
    toJSON mp =
        case mp of
            ModBusTCP -> String "TCP"
            ModBusRTU -> String "RTU"

instance Arbitrary ModbusProtocol where
    arbitrary = elements [ModBusTCP, ModBusRTU]

---------------------------------------------------------------------------------------------------------------
-- Client
---------------------------------------------------------------------------------------------------------------

newtype Client = Client Config

instance ModbusClient Client where
    tcpRunClient worker configMVar session =
        bracket getClient releaseClient $
            \(Client config) ->
                let TCPSession session' = session
                 in TCP.runSession (unTCPWorker worker) config session'
      where
        getClient = liftIO $ takeMVar configMVar
        releaseClient = liftIO . putMVar configMVar

    rtuRunClient worker configMVar session =
        bracket getClient releaseClient $
            \(Client config) ->
                let RTUSession session' = session
                 in RTU.runSession (unRTUWorker worker) config session'
      where
        getClient = liftIO $ takeMVar configMVar
        releaseClient = liftIO . putMVar configMVar

tcpReadInputRegisters ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Range Address ->
    TCPSession m [Word16]
tcpReadInputRegisters tpu range = TCPSession (TCP.readInputRegisters (getTPU tpu) range)

rtuReadInputRegisters ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Range Address ->
    RTUSession m [Word16]
rtuReadInputRegisters tpu range = RTUSession (RTU.readInputRegisters (RTU.UnitId $ getUID tpu) range)

tcpReadHoldingRegisters ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Range Address ->
    TCPSession m [Word16]
tcpReadHoldingRegisters tpu range = TCPSession (TCP.readHoldingRegisters (getTPU tpu) range)

rtuReadHoldingRegisters ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Range Address ->
    RTUSession m [Word16]
rtuReadHoldingRegisters tpu range = RTUSession (RTU.readHoldingRegisters (RTU.UnitId $ getUID tpu) range)

tcpWriteSingleRegister ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Address ->
    Word16 ->
    TCPSession m ()
tcpWriteSingleRegister tpu address value = TCPSession (TCP.writeSingleRegister (getTPU tpu) address value)

rtuWriteSingleRegister ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Address ->
    Word16 ->
    RTUSession m ()
rtuWriteSingleRegister tpu address value = RTUSession (RTU.writeSingleRegister (RTU.UnitId $ getUID tpu) address value)

tcpWriteMultipleRegisters ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Address ->
    [Word16] ->
    TCPSession m ()
tcpWriteMultipleRegisters tpu address values = TCPSession (TCP.writeMultipleRegisters (getTPU tpu) address values)

rtuWriteMultipleRegisters ::
    (MonadIO m, MonadThrow m) =>
    TransactionInfo ->
    Address ->
    [Word16] ->
    RTUSession m ()
rtuWriteMultipleRegisters tpu address values = RTUSession (RTU.writeMultipleRegisters (RTU.UnitId $ getUID tpu) address values)

tcpReadMBRegister ::
    (MonadIO m, MonadThrow m, MBRegister b) =>
    TID ->
    ByteOrder ->
    b ->
    TCPSession m b
tcpReadMBRegister tid bo reg =
    case registerType reg of
        InputRegister ->
            registerFromWord16 bo reg <$> readInput
        HoldingRegister ->
            registerFromWord16 bo reg <$> readHolding
  where
    readInput = TCPSession (TCP.readInputRegisters tpu range)
    readHolding = TCPSession (TCP.readHoldingRegisters tpu range)
    tpu = getRegisterTPU (registerUID reg) tid
    range = registerAddress reg

rtuReadMBRegister ::
    (MonadIO m, MonadThrow m, MBRegister b) =>
    ByteOrder ->
    b ->
    RTUSession m b
rtuReadMBRegister bo reg =
    case registerType reg of
        InputRegister ->
            registerFromWord16 bo reg <$> readInput
        HoldingRegister ->
            registerFromWord16 bo reg <$> readHolding
  where
    readInput = RTUSession (RTU.readInputRegisters (RTU.UnitId $ registerUID reg) range)
    readHolding = RTUSession (RTU.readHoldingRegisters (RTU.UnitId $ registerUID reg) range)
    range = registerAddress reg

tcpWriteMBRegister ::
    (MonadIO m, MonadThrow m, MBRegister b) =>
    TID ->
    ByteOrder ->
    b ->
    TCPSession m ()
tcpWriteMBRegister tid bo reg =
    TCPSession $ case registerType reg of
        InputRegister -> throw $ MB.OtherException "Non writable Register Type"
        HoldingRegister -> TCP.writeMultipleRegisters tpu address values
  where
    values = registerToWord16 bo reg
    tpu = getRegisterTPU (registerUID reg) tid
    address = begin $ registerAddress reg

tcpUpdateMBRegister ::
    (MonadIO m, MonadThrow m, MBRegister b) =>
    TID ->
    ByteOrder ->
    b ->
    TCPSession m b
tcpUpdateMBRegister tid bo reg =
    tcpWriteMBRegister tid bo reg *> tcpReadMBRegister tid bo reg

rtuWriteMBRegister ::
    (MonadIO m, MonadThrow m, MBRegister b) =>
    ByteOrder ->
    b ->
    RTUSession m ()
rtuWriteMBRegister bo reg =
    RTUSession $ case registerType reg of
        InputRegister -> throw $ MB.OtherException "Non writable Register Type"
        HoldingRegister -> RTU.writeMultipleRegisters (RTU.UnitId $ registerUID reg) address values
  where
    values = registerToWord16 bo reg
    address = begin $ registerAddress reg

rtuUpdateMBRegister ::
    (MonadIO m, MonadThrow m, MBRegister b) =>
    ByteOrder ->
    b ->
    RTUSession m b
rtuUpdateMBRegister bo reg =
    rtuReadMBRegister bo reg *> rtuReadMBRegister bo reg

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
    registerUID :: a -> Word8
    registerAddress :: a -> Range Address
    registerToWord16 :: ByteOrder -> a -> [Word16]
    registerFromWord16 :: ByteOrder -> a -> [Word16] -> a

---------------------------------------------------------------------------------------------------------------
-- Worker
---------------------------------------------------------------------------------------------------------------

-- Workers execute Sessions
newtype RTUWorker m = RTUWorker {unRTUWorker :: RTU.Worker m}
newtype TCPWorker m = TCPWorker {unTCPWorker :: TCP.Worker m}

tcpDirectWorker :: TCPWorker IO
tcpDirectWorker = TCPWorker TCP.directWorker

tcpBatchWorker :: TCPWorker IO
tcpBatchWorker = TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig

rtuDirectWorker :: RTUWorker IO
rtuDirectWorker = RTUWorker RTU.directWorker

rtuBatchWorker :: RTUWorker IO
rtuBatchWorker = RTUWorker $ RTU.batchWorker RTU.defaultBatchConfig

---------------------------------------------------------------------------------------------------------------
-- Session
---------------------------------------------------------------------------------------------------------------

-- AN RTU/TCP Session
-- Sessions are executed by workers and can be batched
newtype TCPSession m a = TCPSession {unTCPSession :: TCP.Session m a}
    deriving (Functor, Applicative)

newtype RTUSession m a = RTUSession {unRTUSession :: RTU.Session m a}
    deriving (Functor, Applicative)

---------------------------------------------------------------------------------------------------------------
-- TransactionInfo
---------------------------------------------------------------------------------------------------------------

-- A server is mainly defined by a unit id
-- In case the Modbus TCP protocol is used, every transaction
-- must have its unique id. This is not necessary for Modbus RTU
data TransactionInfo = TransactionInfo
    { unitId :: Word8
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
getTPU (TransactionInfo uid tid) =
    TCP.TPU
        (TCP.TransactionId $ unTID tid)
        TCP.ModbusTcp
        (TCP.UnitId uid)

getRegisterTPU :: Word8 -> TID -> TCP.TPU
getRegisterTPU uid tid =
    TCP.TPU
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
data RegType
    = InputRegister
    | HoldingRegister
    deriving (Eq)

instance Show RegType where
    show InputRegister = "Input Register"
    show HoldingRegister = "Holding Register"

instance Arbitrary RegType where
    arbitrary = elements [InputRegister, HoldingRegister]

instance ToJSON RegType where
    toJSON rt =
        case rt of
            InputRegister -> String "input register"
            HoldingRegister -> String "holding register"

instance FromJSON RegType where
    parseJSON (String s) =
        case s of
            "input register" -> return InputRegister
            "holding register" -> return HoldingRegister
            _ -> fail "Not a RegType"
    parseJSON _ = fail "Not a RegType"

serializeRegType :: RegType -> String
serializeRegType rt =
    case rt of
        InputRegister -> "input register"
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
    = LE
    | BE
    deriving (Show, Read, Eq)

instance Arbitrary ByteOrder where
    arbitrary = elements [LE, BE]

instance ToJSON ByteOrder where
    toJSON bo =
        case bo of
            LE -> String "le"
            BE -> String "be"

instance FromJSON ByteOrder where
    parseJSON (String s) =
        case s of
            "le" -> return LE
            "be" -> return BE
            _ -> fail "Not a ByteOrder"
    parseJSON _ = fail "Not a ByteOrder"

-- Converts a Float to a list of Word16s
float2Word16 :: ByteOrder -> Float -> [Word16]
float2Word16 LE float =
    runGet (combine <$> getWord16host <*> getWord16host) floatle
  where
    floatle = runPut $ putFloatle float
    combine a b = a : [b]
float2Word16 BE float =
    runGet (combine <$> getWord16host <*> getWord16host) floatle
  where
    floatle = runPut $ putFloatle float
    combine a b = b : [a]

-- Converts a list of Word16s to a Maybe Float
word16ToFloat :: ByteOrder -> [Word16] -> Maybe Float
word16ToFloat _ [] = Nothing
word16ToFloat LE [a, b] =
    Just $ runGet getFloatle float
  where
    float = runPut $ putWord16host a >> putWord16host b
word16ToFloat BE [a, b] =
    Just $ runGet getFloatle float
  where
    float = runPut $ putWord16host b >> putWord16host a
word16ToFloat _ _ = Nothing

---------------------------------------------------------------------------------------------------------------
-- HeartBeat Signal
---------------------------------------------------------------------------------------------------------------

-- A HeartBeat signal
data HeartBeat = HeartBeat
    { hbAddress :: !MB.Address -- Address
    -- Unit id
    , hbUid :: !Word8
    , -- Interval in seconds
      hbInterval :: !Int
    , -- ThreadId
      hbThreadId :: !(Maybe ThreadId)
    , -- Status : Empty = Ok , SomeException = thread has panicked
      hbStatus :: MVar SomeException -- Status : Empty = Ok , SomeException = thread has panicked
    }
    deriving (Eq)

-- Spawns a heartbeat signal thread
heartBeatSignal ::
    (MonadIO m, Application m, ModbusClient a, MonadThrow m, MonadMask m) =>
    HeartBeat ->
    Either (TCPWorker m) (RTUWorker m) -> -- Worker to execute the session
    MVar a -> -- Client configuration
    TVar TID ->
    m HeartBeat
heartBeatSignal hb worker clientMVar tid =
    case hbThreadId hb of
        Just _ -> return hb
        Nothing -> do
            let status = hbStatus hb
            let address = hbAddress hb
            let uid = hbUid hb
            let interval = hbInterval hb
            threadid <- liftIO $ forkFinally (execApp $ thread address uid interval 0) (finally status)
            return $ HeartBeat address uid interval (Just threadid) status
  where
    thread address uid interval acc = do
        liftIO $ threadDelay (interval * 1000000) -- in microseconds
        tid' <- liftIO $ getNewTID tid
        let tpu' = setTPU uid tid'
        case worker of
            Left tcpworker ->
                let session = TCPSession (TCP.writeSingleRegister (getTPU tpu') address acc)
                 in tcpRunClient tcpworker clientMVar session
            Right rtuworker ->
                let session = RTUSession (RTU.writeSingleRegister (RTU.UnitId uid) address acc)
                 in rtuRunClient rtuworker clientMVar session
        thread address uid interval (acc + 1)
    finally status terminationStatus =
        case terminationStatus of
            Left someExcept ->
                putMVar status someExcept
            Right () ->
                return ()

newHeartBeat :: Word16 -> Word8 -> Int -> IO HeartBeat
newHeartBeat addr uid tm = do
    status <- liftIO newEmptyMVar
    return $ HeartBeat (MB.Address addr) uid tm Nothing status

---------------------------------------------------------------------------------------------------------------
-- Config
---------------------------------------------------------------------------------------------------------------

-- Connect to the server using a new socket and checking for a timeout
getTCPSocket :: IPv4 -> Int -> Int -> IO (Maybe S.Socket)
getTCPSocket ip port tm = do
    let addr = getAddr ip port
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    (rlt :: Either SomeException (Maybe ())) <- try $ TM.timeout tm (S.connect s addr)
    case rlt of
        Left _ -> return Nothing
        Right m -> return $ s <$ m

-- Connect to the server using a new socket and checking for a timeout
getRTUSerialPort :: String -> SerialSettings -> IO (Maybe SP.SerialPort)
getRTUSerialPort serial settings = do
    let s = SP.openSerial serial (unSR settings)
    let tm = SP.timeout (unSR settings) * 100000 -- from tenths of second to microsecond
    (rlt :: Either SomeException (Maybe SP.SerialPort)) <- try $ TM.timeout tm s
    case rlt of
        Left _ -> return Nothing
        Right m -> return m

getTCPClient :: S.Socket -> Int -> IO (MVar Client)
getTCPClient socket tm = newMVar $ Client $ getTCPConfig socket tm

getRTUClient :: SP.SerialPort -> Int -> IO (MVar Client)
getRTUClient serial tm = newMVar $ Client $ getRTUConfig serial tm

getTCPConfig :: S.Socket -> Int -> Config
getTCPConfig s timeout =
    MB.Config
        { MB.cfgWrite = S.send s
        , MB.cfgRead = S.recv s 4096
        , MB.cfgCommandTimeout = timeout
        , MB.cfgRetryWhen = const . const False
        , MB.cfgEnableBroadcasts = False
        }

getRTUConfig :: SP.SerialPort -> Int -> Config
getRTUConfig s timeout =
    MB.Config
        { MB.cfgWrite = SP.send s
        , MB.cfgRead = SP.recv s 4096
        , MB.cfgCommandTimeout = timeout
        , MB.cfgRetryWhen = const . const False
        , MB.cfgEnableBroadcasts = True
        }

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)

---------------------------------------------------------------------------------------------------------------
-- SerialSettings
---------------------------------------------------------------------------------------------------------------

newtype SerialSettings = SR {unSR :: SP.SerialPortSettings}

buildSerialSettings :: BaudRate -> StopBits -> Parity -> Int -> SerialSettings
buildSerialSettings baudrate stopbits parity tm =
    SR $
        SP.SerialPortSettings
            (unBR baudrate)
            8
            (unSB stopbits)
            (unParity parity)
            SP.NoFlowControl
            tm

instance FromJSON SerialSettings where
    parseJSON (Object o) = do
        baudrate <- o .: "baudrate"
        stopbits <- o .: "stopbits"
        parity <- o .: "parity"
        timeout <- o .: "timeout"
        pure $
            SR $
                SP.SerialPortSettings
                    (unBR baudrate)
                    8
                    (unSB stopbits)
                    (unParity parity)
                    SP.NoFlowControl
                    (timeout `div` 10) -- from seconds to tenths of second
    parseJSON _ = fail "Not a SerialSetting"

instance ToJSON SerialSettings where
    toJSON sr =
        let sps = unSR sr
         in object
                [ "baudrate" .= BR (SP.commSpeed sps)
                , "stopbits" .= SB (SP.stopb sps)
                , "parity" .= Parity (SP.parity sps)
                , "timeout" .= (10 * SP.timeout sps) -- from tenths of second to seconds
                ]

instance Arbitrary SerialSettings where
    arbitrary =
        buildSerialSettings
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

newtype BaudRate = BR {unBR :: SP.CommSpeed}

instance FromJSON BaudRate where
    parseJSON (String s) =
        case s of
            "BR110" -> pure $ BR SP.CS110
            "BR300" -> pure $ BR SP.CS300
            "BR600" -> pure $ BR SP.CS600
            "BR1200" -> pure $ BR SP.CS1200
            "BR2400" -> pure $ BR SP.CS2400
            "BR4800" -> pure $ BR SP.CS4800
            "BR9600" -> pure $ BR SP.CS9600
            "BR19200" -> pure $ BR SP.CS19200
            "BR38400" -> pure $ BR SP.CS38400
            "BR57600" -> pure $ BR SP.CS57600
            "BR115200" -> pure $ BR SP.CS115200
            _ -> fail "Not a BaudRate"
    parseJSON _ = fail "Not a BaudRate"

instance ToJSON BaudRate where
    toJSON br =
        case unBR br of
            SP.CS110 -> String "BR110"
            SP.CS300 -> String "BR300"
            SP.CS600 -> String "BR600"
            SP.CS1200 -> String "BR1200"
            SP.CS2400 -> String "BR2400"
            SP.CS4800 -> String "BR4800"
            SP.CS9600 -> String "BR9600"
            SP.CS19200 -> String "BR19200"
            SP.CS38400 -> String "BR38400"
            SP.CS57600 -> String "BR57600"
            SP.CS115200 -> String "BR115200"

instance Arbitrary BaudRate where
    arbitrary =
        elements
            [ BR SP.CS110
            , BR SP.CS300
            , BR SP.CS600
            , BR SP.CS1200
            , BR SP.CS2400
            , BR SP.CS4800
            , BR SP.CS9600
            , BR SP.CS19200
            , BR SP.CS38400
            , BR SP.CS57600
            , BR SP.CS115200
            ]

newtype StopBits = SB {unSB :: SP.StopBits}

instance FromJSON StopBits where
    parseJSON (String s) =
        case s of
            "one" -> pure $ SB SP.One
            "two" -> pure $ SB SP.Two
            _ -> fail "Not a StopBit"
    parseJSON _ = fail "Not a StopBit"

instance ToJSON StopBits where
    toJSON sb =
        case unSB sb of
            SP.One -> String "one"
            SP.Two -> String "two"

instance Arbitrary StopBits where
    arbitrary =
        elements [SB SP.One, SB SP.Two]

newtype Parity = Parity {unParity :: SP.Parity}

instance FromJSON Parity where
    parseJSON (String s) =
        case s of
            "odd" -> pure $ Parity SP.Odd
            "even" -> pure $ Parity SP.Even
            "none" -> pure $ Parity SP.NoParity
            _ -> fail "Not a Parity"
    parseJSON _ = fail "Not a Parity"

instance ToJSON Parity where
    toJSON parity =
        case unParity parity of
            SP.Odd -> "odd"
            SP.Even -> "even"
            SP.NoParity -> "none"

instance Arbitrary Parity where
    arbitrary =
        elements [Parity SP.Odd, Parity SP.Even]
