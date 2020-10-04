module Main where

import           Control.Monad.Except (runExceptT)
import           Data.Word            (Word8)

import qualified Data.Text.IO         as T
import qualified Network.Socket       as S

import           CsvParser            (parseCSVFile)
import           Modbus               (getAddr, modSession, modbusConnection,
                                       withSocket)
import           OptParser            (AppMode (..), Opt (..), Protocol (..),
                                       runOpts)
import           PrettyPrint          (ppError)
import           Repl                 (runRepl)
import           Server               (runServer)

import           Types                (ByteOrder, Client (..), Config, ModData,
                                       ReplConfig (Config),
                                       ReplState (ReplState), getRTUConfig,
                                       getTCPConfig, serializeModData)


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
runTCPReplApp addr tm order mdata uid = do
    client <- newMVar $ TCPClient config
    withSocket addr $ \s ->
        runRepl (ReplState client mdata uid [] 0 order)
  where
      config = Config
        { MB.cfgWrite = S.send s
        , MB.cfgRead = S.recv s 4096
        , MB.cfgCommandTimeout = tm * 1000
        , MB.cfgRetryWhen = const . const False
        , MB.cfgEnableBroadcasts = False
        }

withSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withSocket addr = bracket (connect addr) close
  where close s = S.gracefulClose s 1000

