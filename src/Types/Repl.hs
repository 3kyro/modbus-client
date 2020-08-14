module Types.Repl 
    (
      Repl
    , ReplState (..)
    , ReplConfig (..)
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

import qualified System.Modbus.TCP as MB

import CsvParser (ModData (..), ByteOrder (..))

type Repl a = HaskelineT (StateT ReplState (ReaderT ReplConfig IO)) a 

data ReplState = ReplState {
    modData :: [ModData]
}

data ReplConfig = Config {
      conn :: MB.Connection
    , ord :: ByteOrder
}

data ReplIdent =
      ReplDesc String
    | ReplAddr Word16
    deriving Show

replAsk :: Repl ReplConfig
replAsk = lift $ lift $ ask
