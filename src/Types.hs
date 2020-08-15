module Types 
    (
      module Types.Repl  
    , module Types.CSV  
    , ReadRegsFun 
    , AppError (..)
    ) 
    where

import Control.Monad.IO.Class ()
import Data.Word (Word16)
import Text.Parsec (ParseError)

import qualified System.Modbus.TCP as MB

import Types.Repl
import Types.CSV
import Control.Exception (IOException)

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16]

data AppError = 
      AppParseError ParseError
    | AppModbusError MB.ModbusException
    | AppCommandError String
    | AppIOError IOException
    deriving (Show)





