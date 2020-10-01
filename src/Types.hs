module Types 
    ( module Types.Repl
    , module Types.ModData
    , module Types.Server
    , module Types.Modbus
    , ReadRegsFun
    , AppError (..)
    ) where

import Control.Exception (Exception, IOException)
import Control.Monad.IO.Class ()
import Data.Word (Word16)
import Text.Parsec (ParseError)

import qualified Network.Modbus.TCP as MB

import Types.Repl
import Types.ModData
import Types.Server
import Types.Modbus

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.Address -> Word16 -> MB.Session IO [Word16]

data AppError = 
      AppParseError ParseError
    | AppModbusError MB.ModbusException
    | AppCommandError String
    | AppIOError IOException

instance Exception AppError

instance Show AppError where
    show (AppParseError err) = "Parse error: " ++ show err
    show (AppModbusError err) = "Modbus error: " ++ show err
    show (AppCommandError err) = "Command error: " ++ show err
    show (AppIOError err) = "I/O error " ++ show err



