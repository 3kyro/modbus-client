{-# LANGUAGE OverloadedStrings #-}
module Types.Server
    ( Server
    , ServState (..)
    , ConnectionData (..)
    ) where

import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import Types.Repl (ThreadState (..))
import Data.IP (IPv4)
import Data.Aeson
    (ToJSON
    , toJSON
    , object
    , (.=)
    , FromJSON
    , parseJSON
    , (.:)
    )
import Data.Aeson.Types (Value (..))
import Servant

import Types.ModData

data ServState = ServState
    { servConn      :: !(Maybe (S.Socket , MB.Connection))
    , servOrd       :: !ByteOrder
    , servPool      :: ![ThreadState]
    , servConnInfo  :: !(Maybe ConnectionData)
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

instance ToJSON ConnectionData where
    toJSON cd = object
        [ "ip address" .= show (servIpAddress cd)
        , "port" .= servPortNum cd
        , "timeout" .= servTimeout cd
        ]
