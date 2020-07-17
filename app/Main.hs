{-|
Module : Main
Description: A Modbus TCP CLI and web server

Modbus-serve is a Modbus TCP command line communication tool and web server.
-}
module Main where

import Control.Monad.Except (runExceptT)
import OptParser (Opt(..), runOpts)
import Data.IP (IPv4, toHostAddress)
import Network.Socket.ByteString (recv, send)


import qualified Data.Text.IO as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import Modbus (modSession)
import CsvParser (ByteOrder (..), ModData, runpCSV)



main :: IO ()
main = greet =<< runOpts

greet :: Opt -> IO ()
greet (Opt input output ip port order) = do
    putStrLn "Parsing register file"
    contents <- T.readFile input
    let md = runpCSV contents
    case md of
        Left err -> fail "Parse error"
        Right md' -> do
            resp <- runApp (getAddr ip port) order md'
            writeFile output (show resp) 

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip port = S.SockAddrInet (fromIntegral port) (toHostAddress ip)

local :: S.Socket -> MB.Connection
local s =
    MB.Connection
    { MB.connWrite          = send s
    , MB.connRead           = recv s
    , MB.connCommandTimeout = 1000
    , MB.connRetryWhen      = \e n -> False
    }

runApp :: S.SockAddr -> ByteOrder -> [ModData] -> IO [ModData]
runApp addr order md = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    resp <- runExceptT $ MB.runSession (local s) (modSession md order)
    case resp of
        Left err -> fail $ "Modbus error: " ++ show err
        Right resp' -> return resp'