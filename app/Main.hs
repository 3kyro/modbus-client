module Main where

import Control.Monad.Except (runExceptT)
import Data.Word (Word8)

import qualified Data.Text.IO as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import CsvParser (parseCSVFile)
import Modbus
    ( modSession
    , modbusConnection
    , withSocket
    , getAddr
    )
import OptParser (Opt(..), AppMode (..), runOpts)
import PrettyPrint (ppError)
import Repl (runRepl)
import Server (runServer)
import Types

main :: IO ()
main = runApp =<< runOpts

runApp :: Opt -> IO ()
runApp (Opt mode input output ip portNum order uid tm) =
    case mode of
        AppTemplate -> do
            parseResult <- parseCSVFile input
            case parseResult of
                Left err -> ppError err
                Right md' -> do
                    resp <- runModDataApp (getAddr ip portNum) tm order md'
                    T.writeFile output (serializeModData resp)
        AppRepl -> runReplApp (getAddr ip portNum) tm order [] uid
        AppWeb -> runServer ip portNum order tm

runModDataApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> IO [ModData]
runModDataApp addr tm order md =
    withSocket addr $ \s -> do
            resp <- runExceptT $ MB.runSession (modbusConnection s tm) (modSession md order)
            case resp of
                Left err -> fail $ "Modbus error: " ++ show err
                Right resp' -> return resp'

-- Run the application's REPL
runReplApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runReplApp addr tm order mdata uid =
    withSocket addr $ \s ->
        runRepl (Config (modbusConnection s tm) addr order tm) (ReplState mdata uid [] 0)


