module Main where

import           Control.Concurrent         (newMVar)
import           Control.Exception.Safe     (bracket)
import           Data.Word                  (Word8)

import qualified Network.Modbus.Protocol    as MB
import qualified Network.Modbus.RTU         as RTU
import qualified Network.Modbus.TCP         as TCP
import qualified Network.Socket             as S
import qualified Network.Socket.ByteString  as S
import qualified System.Hardware.Serialport as SP

import           OptParser                  (AppMode (..), Opt (..), runOpts)
import           Repl                       (runRepl)
-- import           Server               (runServer)
import           CsvParser                  (parseCSVFile)
import           Data.Data                  (Proxy (..))
import           Data.IP                    (IPv4)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text.IO               as T
import           PrettyPrint                (ppError)
import           Types                      (ByteOrder, Client (..), Config,
                                             ModData, ModbusProtocol (..),
                                             ReplState (ReplState), Worker (..),
                                             getAddr, getNewTID, initTID,
                                             readMBRegister, runClient,
                                             serializeModData)


main :: IO ()
main = runApp =<< runOpts

runApp :: Opt -> IO ()
runApp (Opt mode prot input output ip portNum serial order uid tm)
 =
    case mode of
        AppTemplate -> runAppTemplate prot input output ip portNum serial order tm
        AppRepl     -> case prot of
            ModBusTCP -> runTCPReplApp (getAddr ip portNum) tm order [] uid
            ModBusRTU -> runRTUReplApp serial tm order [] uid
        -- AppWeb -> runServer ip portNum order tm
        AppWeb      -> undefined

runAppTemplate :: ModbusProtocol        -- Protocol
               -> FilePath              -- Input File
               -> FilePath              -- Output File
               -> IPv4                  -- IP
               -> Int                   -- Port Number
               -> String                -- Serial Port
               -> ByteOrder             -- Byte Order
               -> Int                   -- Timeout
               -> IO ()
runAppTemplate prot input output ip portNum serial order tm = do
    parseResult <- parseCSVFile input
    case parseResult of
        Left err -> ppError err
        Right md' -> do
            rsp <- case prot of
                ModBusTCP -> runTCPTemplateApp (getAddr ip portNum) prot order tm md'
                ModBusRTU -> runRTUTemplateApp serial prot order tm md'
            T.writeFile output (serializeModData rsp)

runTCPTemplateApp :: S.SockAddr         -- Socket Address
                  -> ModbusProtocol     -- Protocol
                  -> ByteOrder          -- Byte Order
                  -> Int                -- Timeout
                  -> [ModData]
                  -> IO [ModData]
runTCPTemplateApp addr prot order tm mds =
    withSocket addr $ \s -> do
            client <- newMVar $ Client (configTCP tm s)
            inittid <- initTID
            tid <- getNewTID inittid
            let proxy = Proxy :: Proxy Client
            let sessions = map (readMBRegister proxy prot tid order) mds
            resp <- mapM (runClient (TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig) client) sessions
            let validmds = catMaybes  resp
            if length validmds == length mds
            then return validmds
            else fail "Error while reading MODBUS registers"

runRTUTemplateApp :: String      -- Socket Address
                  -> ModbusProtocol     -- Protocol
                  -> ByteOrder          -- Byte Order
                  -> Int                -- Timeout
                  -> [ModData]
                  -> IO [ModData]
runRTUTemplateApp serial prot order tm mds =
    withSerialPort serial $ \s -> do
            client <- newMVar $ Client (configRTU tm s)
            inittid <- initTID
            tid <- getNewTID inittid
            let proxy = Proxy :: Proxy Client
            let sessions = map (readMBRegister proxy prot tid order) mds
            resp <- mapM (runClient (RTUWorker $ RTU.batchWorker RTU.defaultBatchConfig) client) sessions
            let validmds = catMaybes  resp
            if length validmds == length mds
            then return validmds
            else fail "Error while reading MODBUS registers"

-- Run the application's REPL
runTCPReplApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runTCPReplApp addr tm order mdata uid =
    withSocket addr $ \s -> do
        client <- newMVar $ Client (configTCP tm s)
        tid <- initTID
        runRepl ( ReplState
            client
            ModBusTCP
            (TCPWorker TCP.directWorker)
            (TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig)
            order
            mdata
            uid
            []
            tid
            )

configTCP :: Int -> S.Socket -> Config
configTCP tm s  =
    MB.Config
        { TCP.cfgWrite = S.send s
        , TCP.cfgRead = S.recv s 4096
        , TCP.cfgCommandTimeout = tm * 1000
        , TCP.cfgRetryWhen = const . const False
        , TCP.cfgEnableBroadcasts = False
        }

withSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withSocket addr = bracket (connectTCP addr) close
  where close s = S.gracefulClose s 1000

connectTCP :: S.SockAddr -> IO S.Socket
connectTCP addr = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    return s

runRTUReplApp :: String -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runRTUReplApp serial tm order mdata uid =
    withSerialPort serial $ \s -> do
        client <- newMVar $ Client (configRTU tm s)
        tid <- initTID
        runRepl ( ReplState
            client
            ModBusRTU
            (RTUWorker RTU.directWorker)
            (RTUWorker $ RTU.batchWorker RTU.defaultBatchConfig)
            order
            mdata
            uid
            []
            tid
            )

configRTU :: Int -> SP.SerialPort -> Config
configRTU tm s  =
    MB.Config
        { TCP.cfgWrite = SP.send s
        , TCP.cfgRead = SP.recv s 4096
        , TCP.cfgCommandTimeout = tm * 1000
        , TCP.cfgRetryWhen = const . const False
        , TCP.cfgEnableBroadcasts = False
        }

withSerialPort :: String -> (SP.SerialPort -> IO a )-> IO a
withSerialPort s = bracket (connectRTU s) SP.closeSerial

connectRTU :: String -> IO SP.SerialPort
connectRTU s = do
    putStrLn "Connecting ..."
    openedSerial <- SP.openSerial s SP.defaultSerialSettings { SP.commSpeed = SP.CS9600 }
    putStrLn "connected"
    return openedSerial

