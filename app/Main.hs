module Main where

import Control.Monad.Except (runExceptT)
import Data.Word (Word8)

import qualified Data.Text.IO as T
import qualified Network.Socket as S

import CsvParser (parseCSVFile)
import Modbus
    ( modSession
    , modbusConnection
    , withSocket
    , getAddr
    )
import OptParser (Opt(..), AppMode (..), Protocol (..), runOpts)
import PrettyPrint (ppError)
import Repl (runRepl)
import Server (runServer)

import Types
    ( serializeModData
    , ByteOrder
    , ModData
    , ReplConfig(Config)
    , ReplState(ReplState)
    , Client (..)
    , Config
    , getTCPConfig
    , getRTUConfig
    )

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
        AppTemplate -> runAppTemplate input
        AppRepl -> runTCPReplApp (getAddr ip portNum) tm order [] uid
        -- AppWeb -> runServer ip portNum order tm
        AppWeb -> undefined

runAppTemplate :: FilePath -> IO ()
runAppTemplate protocol path = do
    parseResult <- parseCSVFile input
    case parseResult of
        Left err -> ppError err
        Right md' -> do
            rsp <- case protocol of
                TCP -> runTCPTemplateApp (getAddr ip portNum) tm order md'
                RTU -> undefined
            T.writeFile output (serializeModData resp)

runTCPTemplateApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> IO [ModData]
runTCPTemplateApp addr tm order md =
    withSocket addr $ \s -> do
            config <- getTCPConfig s tm
            resp <- runExceptT $ MB.runSession (modbusConnection s tm) (modSession md order)
            case resp of
                Left err -> fail $ "Modbus error: " ++ show err
                Right resp' -> return resp'


Run the application's REPL
runTCPReplApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runTCPReplApp addr tm order mdata uid =
    withSocket addr $ \s ->
        runRepl (Config (modbusConnection s tm) addr order tm) (ReplState mdata uid [] 0)

withSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withSocket addr = bracket (connect addr) close
  where close s = S.gracefulClose s 1000

