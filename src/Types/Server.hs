{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Server
    ( Server
    , ServState (..)
    , ServerActors (..)
    , Connection (..)
    , ConnectionInfo (..)
    , ConnectionRequest (..)
    , getActors
    , getTCPActors
    , getRTUActors
    , KeepAlive (..)
    , KeepAliveResponse (..)
    ) where

import qualified Network.Socket             as S

import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, (.:), (.=))
import           Data.Aeson.Types           (Value (..))
import           Data.IP                    (IPv4)
import           Servant

import           Control.Concurrent         (MVar, ThreadId)
import           Control.Concurrent.STM     (TVar)
import qualified Data.Text                  as T
import           Modbus                     (ByteOrder, Client, HeartBeat,
                                             ModbusProtocol, TID, Worker,
                                             rtuBatchWorker, rtuDirectWorker,
                                             tcpBatchWorker, tcpDirectWorker)
import qualified System.Hardware.Serialport as SP

data ServState = ServState
    { servConnection  :: !Connection
    , servProtocol    :: !ModbusProtocol
    , servOrd         :: !ByteOrder
    , servTID         :: !(TVar TID)
    , servPool        :: ![HeartBeat]
    , servKeepAliveId :: !(Maybe ThreadId)
    }

data ServerActors = ServerActors
    { sclClient       :: !(MVar Client)
    , sclDirectWorker :: !(Worker IO)
    , sclBatchWorker  :: !(Worker IO)
    }

data Connection = TCPConnection
    { tcpSocket         :: !S.Socket
    , tcpConnectionInfo :: !ConnectionInfo
    , tcpActors         :: !ServerActors
    }
    | RTUConnection
    { rtuPort           :: !SP.SerialPort
    , rtuConnectionInfo :: !ConnectionInfo
    , rtuActors         :: !ServerActors
    }
    | NotConnected

getActors :: Connection -> Maybe ServerActors
getActors (TCPConnection _ _ actors) = Just actors
getActors (RTUConnection _ _ actors) = Just actors
getActors NotConnected               = Nothing

getTCPActors :: MVar Client -> ServerActors
getTCPActors client =
    ServerActors
        client
        tcpDirectWorker
        tcpBatchWorker

getRTUActors :: MVar Client -> ServerActors
getRTUActors client =
    ServerActors
        client
        rtuDirectWorker
        rtuBatchWorker

data ConnectionInfo = TCPConnectionInfo
    { tcpIpAddress :: !IPv4
    , tcpPortNum   :: !Int
    , tcpTimeout   :: !Int
    }
    | RTUConnectionInfo
    { rtuAddress :: !String
    , rtuTimeout :: !Int
    }

instance FromJSON ConnectionInfo where
    parseJSON (Object o) = do
        (valueType :: T.Text) <- o .: "connection type"
        case valueType of
            "tcp" -> do
                ip <- o .: "ip address"
                port <- o .: "port"
                tm <- o .: "timeout"
                return $ TCPConnectionInfo (read ip) port tm
            "rtu" -> do
                serial <- o .: "serial port"
                tm <- o .: "timeout"
                return $ RTUConnectionInfo serial tm
    parseJSON _ = fail "Not a valid Connection Info"

instance ToJSON ConnectionInfo where
    toJSON cd =
        case cd of
            TCPConnectionInfo ip port tm-> object
                [ "connection type" .= String "tcp"
                , "ip address" .= show ip
                , "port" .= port
                , "timeout" .= tm
                ]
            RTUConnectionInfo address tm -> object
                [ "connection type" .= String "rtu"
                , "serial port" .= address
                , "timeout" .= tm
                ]

data ConnectionRequest = ConnectionRequest
    { requestInfo      :: !ConnectionInfo
    , requestKeepAlive :: !KeepAlive
    }

instance FromJSON ConnectionRequest where
    parseJSON (Object o) = do
        jsonInfo <- o .: "connection info"
        jsonKeepAlive <- o .: "keep alive"
        return $ ConnectionRequest jsonInfo jsonKeepAlive
    parseJSON _ = fail "Not a valid ConnectionRequest"

data KeepAlive = KeepAlive
    { flag     :: Bool
    , interval :: Int
    }

instance FromJSON KeepAlive where
    parseJSON (Object o) = do
        pflag <- o .: "flag"
        pinterval <- o .: "interval"
        return $ KeepAlive pflag pinterval
    parseJSON _ = fail "Not a valid KeepAlive"


data KeepAliveResponse
        = KeepAliveActivated
        | KeepAliveDisactivated

instance ToJSON KeepAliveResponse where
    toJSON KeepAliveActivated = String "Keep alive activated"
    toJSON KeepAliveDisactivated = String "Keep alive disactivated"
