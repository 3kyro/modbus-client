{-|
Module : Main
Description: A Modbus TCP CLI and web server

Modbus-serve is a Modbus TCP command line communication tool and web server.
-}
module Main where

import Control.Monad.Except (runExceptT)
import OptParser (Opt(..), runOpts)
import Data.IP (IPv4, toHostAddress)
import Text.Parsec.Error (ParseError)
import Network.Socket.ByteString (recv, send)


import qualified Data.Text.IO as T
import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import Modbus (modSession, Config (..))
import CsvParser (ByteOrder (..), ModData, runpCSV)
import Repl (runRepl)

main :: IO ()
main = runApp =<< runOpts

runApp :: Opt -> IO ()
runApp (Opt input output ip portNum order bRepl) = do
  parseResult <- parseCSVFile input  
  if 
    bRepl 
  then 
    case parseResult of
        Left err -> do 
            putStrLn "Error parsing CSV file" 
            print err
            putStrLn "Running Repl without CSV Data"
            runReplApp (getAddr ip portNum) order [] 
        Right mdata -> runReplApp (getAddr ip portNum) order mdata
  else   
    case parseResult of
        Left _ -> putStrLn "Error Parsing CSV file"
        Right md' -> do
            resp <- runModDataApp (getAddr ip portNum) order md'
            writeFile output (show resp) 

parseCSVFile :: FilePath -> IO (Either ParseError [ModData])
parseCSVFile path = do
        putStrLn "Parsing register file"
        contents <- T.readFile path
        return $ runpCSV contents

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)

getConnection :: S.Socket -> MB.Connection
getConnection s =
    MB.Connection
    { MB.connWrite          = send s
    , MB.connRead           = recv s
    , MB.connCommandTimeout = 1000
    , MB.connRetryWhen      = \_ _ -> False
    }

runModDataApp :: S.SockAddr -> ByteOrder -> [ModData] -> IO [ModData]
runModDataApp addr order md = do
    s <- connect addr
    resp <- runExceptT $ MB.runSession (getConnection s) (modSession md order)
    case resp of
        Left err -> fail $ "Modbus error: " ++ show err
        Right resp' -> return resp'

runReplApp :: S.SockAddr -> ByteOrder -> [ModData] ->IO ()
runReplApp addr order mdata = do
    s <- connect addr
    runRepl $ Config (getConnection s) order mdata

connect :: S.SockAddr -> IO S.Socket
connect addr = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    return s