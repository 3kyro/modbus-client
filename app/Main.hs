module Main where

import Control.Monad.Except (runExceptT)
import Control.Exception.Safe (bracket)
import Data.Word (Word8)
import Data.IP (IPv4, toHostAddress)
import Network.Socket.ByteString (recv, send)
import OptParser (Opt(..), runOpts)

import qualified Data.Text.IO as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import CsvParser (parseCSVFile)
import Modbus (modSession)
import PrettyPrint (ppError)
import Repl (runRepl)
import Types

main :: IO ()
main = runApp =<< runOpts

runApp :: Opt -> IO ()
runApp (Opt input output ip portNum order bRepl uid tm) =
  if bRepl
  then runReplApp (getAddr ip portNum) tm order [] uid
  else do
    parseResult <- parseCSVFile input
    case parseResult of
        Left err -> ppError err
        Right md' -> do
            resp <- runModDataApp (getAddr ip portNum) tm order md'
            T.writeFile output (serializeModData resp)

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)

modbusConnection :: S.Socket -> Int -> MB.Connection
modbusConnection s tm =
    MB.Connection
    { MB.connWrite          = send s
    , MB.connRead           = recv s
    , MB.connCommandTimeout = tm * 1000
    , MB.connRetryWhen      = const . const False
    }

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
        runRepl (Config (modbusConnection s tm) addr order tm) (ReplState mdata uid [])

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

