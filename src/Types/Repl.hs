module Types.Repl 
    ( Repl
    , ReplState (..)
    , ReplConfig (..)
    , ReplArg (..)
    , ThreadState (..)
    , replAsk
    , Command (..)
    ) where

import Control.Concurrent (MVar, ThreadId)
import Control.Exception (SomeException)
import Control.Monad.IO.Class ()
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Writer (MonadTrans(lift))
import Data.Word (Word8, Word16)
import System.Console.Repline (HaskelineT)

import qualified Network.Socket as S
import qualified Network.Modbus.TCP as MB

import Types.ModData (ModData (..), ByteOrder (..))

type Repl a = HaskelineT (StateT ReplState (ReaderT ReplConfig IO)) a

-- The state of active heartbeat threads.
-- threadAddr = The register address of a current active heartbeat
-- threadId = Thread id for the active heartbeat
-- threadMVar = MVar showing the status of the thread:
--      Empty -> thread is still running
--      SomeException -> thread has encoutered some exception
data ThreadState = ThreadState
    { threadAddr :: Word16
    , threadInterv :: Int
    , threadId :: ThreadId
    , threadMVar :: MVar SomeException
    } deriving (Eq)

data ReplState = ReplState
    { replModData       :: ![ModData]
    , replUId           :: !Word8
    , replPool          :: ![ThreadState]
    , replTransactionId :: !Word16
    }

data ReplConfig = Config
    { replConn       :: !MB.Connection
    , replSockAddr   :: !S.SockAddr
    , replOrd        :: !ByteOrder
    , replTimeout    :: !Int
    }

-- Defines the type of an argument in certain repl commands
data ReplArg
    = ReplName String   -- Argument is a name
    | ReplAddr Word16   -- Argument is an address
    deriving Show

replAsk :: Repl ReplConfig
replAsk = lift $ lift ask

data Command
    = ReadInputRegistersWord
    | ReadInputRegistersFloat
    | ReadHoldingRegistersWord
    | ReadHoldingRegistersFloat
    | WriteRegistersWord
    | WriteRegistersFloat
    | Read
    | Write
    | Heartbeat
    | StopHeartbeat
    | ListHeartbeat
    | Import
    | Export
    | Id
    | CommandNotFound
    deriving (Eq, Show)