{-# LANGUAGE OverloadedStrings #-}
module Types.Server
    ( Server
    , ServState (..)
    , ConnectionData (..)
    ) where

import qualified Network.Socket   as S

import           Data.Aeson       (FromJSON, ToJSON, object, parseJSON, toJSON,
                                   (.:), (.=))
import           Data.Aeson.Types (Value (..))
import           Data.IP          (IPv4)
import           Servant

import           Types.Modbus     (ByteOrder, Config, HeartBeat)

data ServState = ServState
    { servConn     :: !(Maybe (S.Socket, Config))
    , servOrd      :: !ByteOrder
    , servPool     :: ![HeartBeat]
    , servConnInfo :: !(Maybe ConnectionData)
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
