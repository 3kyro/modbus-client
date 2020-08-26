{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module : Main
Description: A Modbus TCP CLI and web server

Modbus-serve is a Modbus TCP command line communication tool and web server.
-}
module Main where

import Control.Monad.Except (runExceptT)
import Data.Word (Word8)
import OptParser (Opt(..), runOpts)
import Data.IP (IPv4, toHostAddress)
import Network.Socket.ByteString (recv, send)

import qualified Data.Text.IO as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB


import PrettyPrint (ppError)
import Modbus (modSession)
import CsvParser (parseCSVFile)
import Types
    (
      serializeModData
    , ByteOrder (..)
    , ModData
    , ReplConfig(..)
    , ReplState(..)
    )
import Repl (runRepl)

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

getConnection :: S.Socket -> Int -> MB.Connection
getConnection s tm =
    MB.Connection
    { MB.connWrite          = send s
    , MB.connRead           = recv s
    , MB.connCommandTimeout = tm * 1000
    , MB.connRetryWhen      = \_ _ -> False
    }

runModDataApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> IO [ModData]
runModDataApp addr tm order md = do
    s <- connect addr
    resp <- runExceptT $ MB.runSession (getConnection s tm) (modSession md order)
    case resp of
        Left err -> fail $ "Modbus error: " ++ show err
        Right resp' -> return resp'

runReplApp :: S.SockAddr -> Int -> ByteOrder -> [ModData] -> Word8 -> IO ()
runReplApp addr tm order mdata uid = do
    s <- connect addr
    runRepl (Config (getConnection s tm) order tm) (ReplState mdata uid [])

connect :: S.SockAddr -> IO S.Socket
connect addr = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    return s