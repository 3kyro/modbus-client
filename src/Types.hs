module Types 
    ( module Types.Repl
    , module Types.ModData
    , ReadRegsFun
    , AppError (..)
    ) where

import Control.Exception (Exception, IOException)
import Control.Monad.IO.Class ()
import Data.Word (Word16)
import Text.Parsec (ParseError)

import qualified System.Modbus.TCP as MB

import Types.Repl
import Types.ModData

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16]

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



