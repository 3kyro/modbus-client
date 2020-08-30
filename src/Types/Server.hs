module Types.Server
    ( Server
    , ServState (..)
    ) where

import Control.Monad.Trans.State.Strict (StateT)

import qualified System.Modbus.TCP as MB

import Types.ModData (ByteOrder (..))
import Types.Repl (ThreadState (..))
import Web.Scotty (ScottyM)


type Server a = StateT ServState ScottyM a

data ServState = ServState
    { servConn  :: !(Maybe MB.Connection)
    , servOrd   :: !ByteOrder
    , servPool  :: ![ThreadState]
    }

