module Main where

import           Control.Concurrent         (newMVar)
import           Control.Exception.Safe     (bracket)
import           Data.IP                    (IPv4)
import           Data.Word                  (Word8)
import           Network.Socket.KeepAlive   (KeepAlive (KeepAlive),
                                             setKeepAlive)

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
import           Types                      (AppError (..), ModData,
                                             ReplState (ReplState),
                                             serializeModData)


main :: IO ()
main = runApp =<< runOpts

runApp :: Opt -> IO ()
runApp opts =
-- runApp (Opt mode prot input output ip portNum serial order uid tm kaonoff kaidle kaintvl) =
  let
    mode = appMode opts
    prot = protocol opts
    input = inputTemplate opts
    output = outputFile opts
    ip = ipAddr opts
    portNum = port opts
    serial = serialPort opts
    order = byteOrder opts
    uid = uId opts
    tms = timeout opts * 1000000 -- timeout in microseconds
    rate = baudrate opts
    stop = stopbits opts
    par = parity opts
    serialSettings = buildSerialSettings rate stop par tms
    kaonoff = kaOnOff opts
    kaidle = kaIdle opts
    kaintvl = kaIntv opts
  in
    case mode of
        AppTemplate -> runAppTemplate prot input output ip portNum serial order tms serialSettings
        AppRepl     -> case prot of
            ModBusTCP -> runTCPReplApp (getAddr ip portNum) tms order [] uid $ KeepAlive kaonoff kaidle kaintvl
            ModBusRTU -> runRTUReplApp serial tms serialSettings order [] uid
        AppWeb -> runServer prot order


---------------------------------------------------------------------------------------------------------------
-- Template App
---------------------------------------------------------------------------------------------------------------


runAppTemplate :: ModbusProtocol        -- Protocol
               -> FilePath              -- Input File
               -> FilePath              -- Output File
               -> IPv4                  -- IP
               -> Int                   -- Port Number
               -> String                -- Serial Port
               -> ByteOrder             -- Byte Order
               -> Int                   -- Timeout
               -> SerialSettings        -- Serial settings
               -> IO ()
runAppTemplate prot input output ip portNum serial order tm settings = do
    parseResult <- parseCSVFile input
    case parseResult of
        Left err -> ppError err
        Right md' -> do
            rsp <- case prot of
                ModBusTCP -> runTCPTemplateApp (getAddr ip portNum) order tm md'
                ModBusRTU -> runRTUTemplateApp serial order tm settings md'
            T.writeFile output (serializeModData rsp)

runTCPTemplateApp :: S.SockAddr         -- Socket Address
                  -> ByteOrder          -- Byte Order
                  -> Int                -- Timeout in microseconds
                  -> [ModData]
                  -> IO [ModData]
runTCPTemplateApp addr order tm mds =
    withSocket addr $ \s -> do
            client <- newMVar $ Client (getTCPConfig s tm)
            inittid <- initTID
            tid <- getNewTID inittid
            let sessions = traverse (tcpReadMBRegister tid order) mds
            tcpRunClient tcpBatchWorker client sessions

runRTUTemplateApp :: String             -- Serial Port
                  -> ByteOrder          -- Byte Order
                  -> Int                -- Timeout in microseconds
                  -> SerialSettings
                  -> [ModData]
                  -> IO [ModData]
runRTUTemplateApp serial order tm settings mds =
    withSerialPort serial settings $ \s -> do
            client <- newMVar $ Client (getRTUConfig s tm)
            let sessions = traverse (rtuReadMBRegister order) mds
            rtuRunClient rtuBatchWorker client sessions


---------------------------------------------------------------------------------------------------------------
-- Repl
---------------------------------------------------------------------------------------------------------------

-- Run the application's REPL
runTCPReplApp ::
    S.SockAddr
    -> Int                  -- timeout in microseconds
    -> ByteOrder
    -> [ModData]
    -> Word8                -- unitid
    -> KeepAlive
    -> IO ()
runTCPReplApp addr tm order mdata uid ka =
    withSocket addr $ \s -> do
        client <- newMVar $ Client (getTCPConfig s tm)
        S.withFdSocket s $ \fd -> do
            rlt <- setKeepAlive fd ka
            case rlt of
                Left err -> ppError $ AppKeepAliveError err
                Right () -> return ()
        tid <- initTID
        runRepl ( ReplState
            client
            ModBusTCP
            (TCPWorker TCP.directWorker)
            (TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig)
            (RTUWorker RTU.directWorker)
            (RTUWorker $ RTU.batchWorker RTU.defaultBatchConfig)
            order
            mdata
            uid
            []
            tid
            )

runRTUReplApp :: String -> Int -> SerialSettings -> ByteOrder -> [ModData] -> Word8 -> IO ()
runRTUReplApp serial tm settings order mdata uid =
    withSerialPort serial settings $ \s -> do
        client <- newMVar $ Client (getRTUConfig s tm)
        tid <- initTID
        runRepl ( ReplState
            client
            ModBusRTU
            (TCPWorker TCP.directWorker)
            (TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig)
            (RTUWorker RTU.directWorker)
            (RTUWorker $ RTU.batchWorker RTU.defaultBatchConfig)
            order
            mdata
            uid
            []
            tid
            )

---------------------------------------------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------------------------------------------

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

withSerialPort :: String -> SerialSettings -> (SP.SerialPort -> IO a )-> IO a
withSerialPort str settings = bracket (connectRTU str settings) SP.closeSerial

connectRTU :: String -> SerialSettings -> IO SP.SerialPort
connectRTU str settings = do
    putStrLn "Connecting ..."
    openedSerial <- SP.openSerial str $ unSR settings
    putStrLn "connected"
    return openedSerial

