module Repl.Types 
    (
      Repl
    , ReadRegsFun 
    , ReplError (..)
    ) 
    where

import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class ()
import Data.Word (Word16)
import System.Console.Repline (HaskelineT)
import Text.Parsec (ParseError)

import qualified System.Modbus.TCP as MB

import Modbus (Config (..))

type Repl a = HaskelineT (ReaderT Config IO) a 

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16]

data ReplError = 
      ReplParseError ParseError
    | ReplModbusError MB.ModbusException
    | ReplCommandError String
    deriving (Show)