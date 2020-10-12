module Main where

import           Control.Concurrent         (newMVar)
import           Control.Exception.Safe     (bracket)
import           Data.Data                  (Proxy (..))
import           Data.IP                    (IPv4)
import           Data.Word                  (Word8)

import qualified Data.Text.IO               as T
import qualified Network.Modbus.RTU         as RTU
import qualified Network.Modbus.TCP         as TCP
import qualified Network.Socket             as S
import qualified System.Hardware.Serialport as SP

import           CsvParser                  (parseCSVFile)
import           Modbus
import           OptParser                  (AppMode (..), Opt (..), runOpts)
import           PrettyPrint                (ppError)
import           Repl                       (runRepl)
import           Server                     (runServer)
import           Types                      (ModData, ReplState (ReplState),
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
        AppWeb -> runServer prot order

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
            client <- newMVar $ Client (getTCPConfig s tm)
            inittid <- initTID
            tid <- getNewTID inittid
            let proxy = Proxy :: Proxy Client
            let sessions = map (readMBRegister proxy prot tid order) mds
            mapM (runClient (TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig) client) sessions

runRTUTemplateApp :: String      -- Socket Address
                  -> ModbusProtocol     -- Protocol
                  -> ByteOrder          -- Byte Order
                  -> Int                -- Timeout
                  -> [ModData]
                  -> IO [ModData]
runRTUTemplateApp serial prot order tm mds =
    withSerialPort serial $ \s -> do
            client <- newMVar $ Client (getRTUConfig s tm)
            inittid <- initTID
            tid <- getNewTID inittid
            let proxy = Proxy :: Proxy Client
            let sessions = map (readMBRegister proxy prot tid order) mds
            mapM (runClient (RTUWorker $ RTU.batchWorker RTU.defaultBatchConfig) client) sessions


-- Run the application's REPL
runTCPReplApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runTCPReplApp addr tm order mdata uid =
    withSocket addr $ \s -> do
        client <- newMVar $ Client (getTCPConfig s tm)
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
        client <- newMVar $ Client (getRTUConfig s tm)
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



withSerialPort :: String -> (SP.SerialPort -> IO a )-> IO a
withSerialPort s = bracket (connectRTU s) SP.closeSerial

connectRTU :: String -> IO SP.SerialPort
connectRTU s = do
    putStrLn "Connecting ..."
    openedSerial <- SP.openSerial s SP.defaultSerialSettings { SP.commSpeed = SP.CS9600 }
    putStrLn "connected"
    return openedSerial

