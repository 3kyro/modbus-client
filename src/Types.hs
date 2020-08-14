module Types 
    (
      Repl
    , ReadRegsFun 
    , ReplError (..)
    , ReplConfig (..)
    , ReplState (..)
    , ReplIdent (..)
    , replAsk
    ) 
    where

import Control.Monad.IO.Class ()
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Writer (MonadTrans(lift))
import Data.Word (Word16)
import System.Console.Repline (HaskelineT)
import Text.Parsec (ParseError)

import qualified System.Modbus.TCP as MB

import CsvParser (ModData (..), ByteOrder (..))

type Repl a = HaskelineT (StateT ReplState (ReaderT ReplConfig IO)) a 

replAsk :: Repl ReplConfig
replAsk = lift $ lift $ ask

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16]

data ReplError = 
      ReplParseError ParseError
    | ReplModbusError MB.ModbusException
    | ReplCommandError String
    deriving (Show)

data ReplConfig = Config {
      conn :: MB.Connection
    , ord :: ByteOrder
}

data ReplState = ReplState {
    modData :: [ModData]
}

data ReplIdent =
      ReplDesc String
    | ReplAddr Word16
    deriving Show