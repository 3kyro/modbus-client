module Types
    ( module Types.Repl
    , module Types.ModData
    , module Types.Server
    , ReadRegsFun
    , AppError (..)
    ) where

import           Control.Exception        (Exception, IOException,
                                           SomeException)

import           Control.Monad.IO.Class   ()
import           Data.Word                (Word16)
import qualified Network.Modbus.TCP       as MB
import           Network.Socket.KeepAlive (KeepAliveError)
import           Text.Parsec              (ParseError)

import           Types.ModData
import           Types.Repl
import           Types.Server

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.Address -> Word16 -> MB.Session IO [Word16]

data AppError = AppParseError ParseError
    | AppModbusError SomeException
    | AppCommandError String
    | AppIOError IOException
    | AppKeepAliveError KeepAliveError

instance Exception AppError

instance Show AppError where
    show (AppParseError err)   = "Parse error: " ++ show err
    show (AppModbusError err)  = "Modbus error: " ++ show err
    show (AppCommandError err) = "Command error: " ++ show err
    show (AppIOError err)      = "I/O error: " ++ show err
    show (AppKeepAliveError err) = "Keep alive error: " ++ show err



