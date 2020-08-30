{-# LANGUAGE OverloadedStrings #-}
module Types.Server
    ( Server
    , ServState (..)
    , ConnectionData (..)
    ) where

import Control.Monad.Trans.State.Strict (StateT)

import qualified System.Modbus.TCP as MB

import Types.ModData (ByteOrder (..))
import Types.Repl (ThreadState (..))
import Web.Scotty (ScottyM)
import Data.IP (IPv4)
import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (Value (..))


type Server a = StateT ServState ScottyM a

data ServState = ServState
    { servConn  :: !(Maybe MB.Connection)
    , servOrd   :: !ByteOrder
    , servPool  :: ![ThreadState]
    }


data ConnectionData = ConnectionData
    { servIpAddress :: !IPv4
    , servPortNum   :: !Int
    , servTimeout   :: !Int
    }

instance FromJSON ConnectionData where
    parseJSON (Object o) = do
        ip <- o .: "ip address"
        port <- o .: "port"
        tm <- o .: "timeout"
        return $ ConnectionData (read ip) port tm
