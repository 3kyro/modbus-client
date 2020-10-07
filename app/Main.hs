module Main where

import           Data.Word                 (Word8)

import qualified Network.Modbus.Protocol   as MB
import qualified Network.Modbus.TCP        as TCP
import qualified Network.Socket            as S

import           OptParser                 (AppMode (..), Opt (..), runOpts)
import           Repl                      (runRepl)
-- import           Server               (runServer)

import           Control.Concurrent        (newMVar)
import qualified Network.Socket.ByteString as S
import           Types                     (getAddr, ByteOrder, Client (..), ModbusProtocol (..),
                                            ModData, ReplState (ReplState), Worker(..))
import Control.Exception.Safe (bracket)


main :: IO ()
main = runApp =<< runOpts

-- getConfig :: Protocol -> Int -> IO Config
-- getConfig protocol timeout =
--     case protocol of
--         TCP -> tcpInit
--         RTU -> rtuInit

-- tcpInit :: Int -> IO Config
-- tcpInit timeout = do



runApp :: Opt -> IO ()
runApp (Opt mode protocol input output ip portNum order uid tm) =
    case mode of
        -- AppTemplate -> runAppTemplate input
        AppTemplate -> undefined
        AppRepl     -> runTCPReplApp (getAddr ip portNum) tm order [] uid
        -- AppWeb -> runServer ip portNum order tm
        AppWeb      -> undefined

-- runAppTemplate :: FilePath -> IO ()
-- runAppTemplate protocol path = do
--     parseResult <- parseCSVFile input
--     case parseResult of
--         Left err -> ppError err
--         Right md' -> do
--             rsp <- case protocol of
--                 TCP -> runTCPTemplateApp (getAddr ip portNum) tm order md'
--                 RTU -> undefined
--             T.writeFile output (serializeModData resp)

-- runTCPTemplateApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> IO [ModData]
-- runTCPTemplateApp addr tm order md =
--     withSocket addr $ \s -> do
--             config <- getTCPConfig s tm
--             resp <- runExceptT $ MB.runSession (modbusConnection s tm) (modSession md order)
--             case resp of
--                 Left err    -> fail $ "Modbus error: " ++ show err
--                 Right resp' -> return resp'


-- Run the application's REPL
runTCPReplApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runTCPReplApp addr tm order mdata uid =
    withSocket addr $ \s -> do
        client <- newMVar $ Client (config s)
        runRepl ( ReplState
            client
            ModBusTCP
            (TCPWorker TCP.directWorker)
            (TCPWorker $ TCP.batchWorker TCP.defaultBatchConfig)
            order
            mdata
            uid
            []
            0
            )
  where
      config s = MB.Config
        { TCP.cfgWrite = S.send s
        , TCP.cfgRead = S.recv s 4096
        , TCP.cfgCommandTimeout = tm * 1000
        , TCP.cfgRetryWhen = const . const False
        , TCP.cfgEnableBroadcasts = False
        }

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