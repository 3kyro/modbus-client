module Types.Repl 
    (
      Repl 
    , ReplState (..)
    , ReplConfig (..)
    , ReplArg (..)
    , replAsk
    )
    where

import Control.Monad.IO.Class ()
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Writer (MonadTrans(lift))
import Data.Word (Word8, Word16)
import System.Console.Repline (HaskelineT)

import qualified System.Modbus.TCP as MB

import Types.ModData (ModData (..), ByteOrder (..))

type Repl a = HaskelineT (StateT ReplState (ReaderT ReplConfig IO)) a 

data ReplState = ReplState {
     replModData    :: ![ModData]
    ,replUId        :: !Word8
}

data ReplConfig = Config {
      conn :: MB.Connection
    , ord :: ByteOrder
}

-- Defines the type of an argument in certain repl commands
data ReplArg =
      ReplName String   -- Argument is a name
    | ReplAddr Word16   -- Argument is an address
    deriving Show

replAsk :: Repl ReplConfig
replAsk = lift $ lift ask
