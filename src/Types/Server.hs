{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Server (
    Server,
    ServState (..),
    ServerActors (..),
    Connection (..),
    ConnectionInfo (..),
    ConnectionRequest (..),
    getActors,
    setActors,
    KeepAliveServ (..),
    KeepAliveResponse (..),
    toKeepAlive,
    OS (..),
    InitRequest (..),
    HeartBeatRequest (..),
    fromHeartBeatRequest,
    toHeartBeatRequest,
) where

import qualified Network.Socket as S

import Data.Aeson (
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    (.:),
    (.=),
 )
import Data.Aeson.Types (Value (..))
import Data.IP (IPv4)
import Servant

import Control.Concurrent (MVar, newEmptyMVar)
import Control.Concurrent.STM (TVar)
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Word (Word32, Word16, Word8)
import Modbus (
    ByteOrder,
    Client,
    HeartBeat (..),
    ModbusProtocol,
    RTUWorker,
    SerialSettings,
    TCPWorker (..),
    TID,
    rtuBatchWorker,
    rtuDirectWorker,
    tcpBatchWorker,
    tcpDirectWorker,
 )
import qualified Network.Modbus.Protocol as MB
import Network.Socket.KeepAlive (KeepAlive (..))
import qualified System.Hardware.Serialport as SP
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, oneof)

---------------------------------------------------------------------------------------------------------------
-- ServState
---------------------------------------------------------------------------------------------------------------

data ServState = ServState
    { servConnection :: !Connection
    , servProtocol :: !ModbusProtocol
    , servOrd :: !ByteOrder
    , servTID :: !(TVar TID)
    , servPool :: [(Word32, HeartBeat)]
    }

---------------------------------------------------------------------------------------------------------------
-- Connection
---------------------------------------------------------------------------------------------------------------

data Connection
    = TCPConnection
        { tcpSocket :: !S.Socket
        , tcpConnectionInfo :: !ConnectionInfo
        , tcpActors :: !ServerActors
        }
    | RTUConnection
        { rtuPort :: !SP.SerialPort
        , rtuConnectionInfo :: !ConnectionInfo
        , rtuActors :: !ServerActors
        }
    | NotConnected

---------------------------------------------------------------------------------------------------------------
-- Server Actors
---------------------------------------------------------------------------------------------------------------

data ServerActors = ServerActors
    { sclClient :: !(MVar Client)
    , sclTCPDirectWorker :: !(TCPWorker IO)
    , sclTCPBatchWorker :: !(TCPWorker IO)
    , sclRTUDirectWorker :: !(RTUWorker IO)
    , sclRTUBatchWorker :: !(RTUWorker IO)
    }

getActors :: Connection -> Maybe ServerActors
getActors (TCPConnection _ _ actors) = Just actors
getActors (RTUConnection _ _ actors) = Just actors
getActors NotConnected = Nothing

setActors :: MVar Client -> ServerActors
setActors client =
    ServerActors
        client
        tcpDirectWorker
        tcpBatchWorker
        rtuDirectWorker
        rtuBatchWorker

---------------------------------------------------------------------------------------------------------------
-- Connection Info
---------------------------------------------------------------------------------------------------------------

data ConnectionInfo
    = TCPConnectionInfo
        { tcpIpAddress :: !IPv4
        , tcpPortNum :: !Int
        , -- in seconds
          tcpTimeout :: !Int -- in seconds
        }
    | RTUConnectionInfo
        { rtuAddress :: !String
        , serialSettings :: !SerialSettings
        } deriving (Eq)

instance Show ConnectionInfo where
    show ci =
        case ci of
            TCPConnectionInfo addr port tm ->
                "TCPConnectionInfo: "
                ++ show addr
                ++ ":"
                ++ show port
                ++ ", timeout:"
                ++ show tm
                ++ " sec"
            RTUConnectionInfo port _ ->
                "RTUConnectionInfo: "
                ++ port


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
                settings <- o .: "settings"
                return $ RTUConnectionInfo serial settings
            _ -> fail "Not a valid Connection Info"
    parseJSON _ = fail "Not a valid Connection Info"

instance ToJSON ConnectionInfo where
    toJSON cd =
        case cd of
            TCPConnectionInfo ip port tm ->
                object
                    [ "connection type" .= String "tcp"
                    , "ip address" .= show ip
                    , "port" .= port
                    , "timeout" .= tm
                    ]
            RTUConnectionInfo address settings ->
                object
                    [ "connection type" .= String "rtu"
                    , "serial port" .= address
                    , "settings" .= settings
                    ]

instance Arbitrary ConnectionInfo where
    arbitrary = oneof [arbTCP, arbRTU]
      where
        arbTCP = TCPConnectionInfo <$> ip4 <*> arbitrary <*> arbitrary
        arbRTU = RTUConnectionInfo <$> arbitrary <*> arbitrary
        ip4 :: Gen IPv4
        ip4 = read <$> showIp
        byte :: Gen Word8
        byte = arbitrary
        showIp = concat <$> sequenceA (intersperse (pure ".") (replicate 4 (show <$> byte)))

---------------------------------------------------------------------------------------------------------------
-- Connection Request
---------------------------------------------------------------------------------------------------------------

data ConnectionRequest = ConnectionRequest
    { requestInfo :: !ConnectionInfo
    , requestKeepAlive :: !KeepAliveServ
    }

instance FromJSON ConnectionRequest where
    parseJSON (Object o) = do
        jsonInfo <- o .: "connection info"
        jsonKeepAlive <- o .: "keep alive"
        return $ ConnectionRequest jsonInfo jsonKeepAlive
    parseJSON _ = fail "Not a valid ConnectionRequest"

instance ToJSON ConnectionRequest where
    toJSON cr =
        object
            [ "connection info" .= requestInfo cr
            , "keep alive" .= requestKeepAlive cr
            ]

instance Arbitrary ConnectionRequest where
    arbitrary = ConnectionRequest <$> arbitrary <*> arbitrary

---------------------------------------------------------------------------------------------------------------
-- InitRequest
---------------------------------------------------------------------------------------------------------------

data InitRequest = InitRequest
    { initConnInfo :: !(Maybe ConnectionInfo)
    , initOs :: !OS
    }

instance FromJSON InitRequest where
    parseJSON (Object o) = do
        info <- o .: "connection info"
        os <- o .: "os"
        pure $ InitRequest info os
    parseJSON _ = fail "Not a InitRequest"

instance ToJSON InitRequest where
    toJSON ir =
        object
            [ "connection info" .= initConnInfo ir
            , "os" .= initOs ir
            ]

instance Arbitrary InitRequest where
    arbitrary = InitRequest <$> arbitrary <*> arbitrary

---------------------------------------------------------------------------------------------------------------
-- Keep Alive
---------------------------------------------------------------------------------------------------------------

data KeepAliveServ = KeepAliveServ
    { flag :: Bool
    , idle :: Word32
    , interval :: Word32
    }
    deriving (Show)

instance FromJSON KeepAliveServ where
    parseJSON (Object o) = do
        pflag <- o .: "flag"
        pidle <- o .: "idle"
        pinterval <- o .: "interval"
        return $ KeepAliveServ pflag pidle pinterval
    parseJSON _ = fail "Not a valid KeepAlive"

instance ToJSON KeepAliveServ where
    toJSON kas =
        object
            [ "flag" .= flag kas
            , "idle" .= idle kas
            , "interval" .= interval kas
            ]

instance Arbitrary KeepAliveServ where
    arbitrary = KeepAliveServ <$> arbitrary <*> arbitrary <*> arbitrary

data KeepAliveResponse
    = KeepAliveActivated
    | KeepAliveDisactivated
    deriving (Show, Eq)

instance FromJSON KeepAliveResponse where
    parseJSON (String s) =
        case s of
            "Keep alive activated" -> pure KeepAliveActivated
            "Keep alive disactivated" -> pure KeepAliveDisactivated
            _ -> fail "Not a KeepAliveResponse"
    parseJSON _ = fail "Not a KeepAliveResponse"

instance ToJSON KeepAliveResponse where
    toJSON KeepAliveActivated = String "Keep alive activated"
    toJSON KeepAliveDisactivated = String "Keep alive disactivated"

instance Arbitrary KeepAliveResponse where
    arbitrary = elements [KeepAliveActivated, KeepAliveDisactivated]

toKeepAlive :: KeepAliveServ -> KeepAlive
toKeepAlive (KeepAliveServ flag' tidle tintv) =
    KeepAlive flag' (fromIntegral tidle) (fromIntegral tintv)

---------------------------------------------------------------------------------------------------------------
-- OS
---------------------------------------------------------------------------------------------------------------

data OS
    = Linux
    | Windows
    | Other

instance FromJSON OS where
    parseJSON (String s) =
        case s of
            "linux" -> pure Linux
            "windows" -> pure Windows
            "other" -> pure Other
            _ -> fail "Not an OS"
    parseJSON _ = fail "Not an OS"

instance ToJSON OS where
    toJSON Linux = String "linux"
    toJSON Windows = String "windows"
    toJSON Other = String "other"

instance Arbitrary OS where
    arbitrary = elements [Linux, Windows, Other]

---------------------------------------------------------------------------------------------------------------
-- HeartBeat
---------------------------------------------------------------------------------------------------------------

data HeartBeatRequest = HeartBeatRequest
    { hbrAddress :: Word16
    , hbrUid :: Word8
    , hbrInterval :: Int
    , hbrId :: Word32
    }

instance ToJSON HeartBeatRequest where
    toJSON hb =
        object
            [ "uid" .= hbrUid hb
            , "address" .= hbrAddress hb
            , "interval" .= hbrInterval hb
            , "id" .= hbrId hb
            ]

instance FromJSON HeartBeatRequest where
    parseJSON (Object o) = do
        pUid <- o .: "uid"
        pAddr <- o .: "address"
        pIntv <- o .: "interval"
        pId <- o .: "id"
        return $ HeartBeatRequest pAddr pUid pIntv pId
    parseJSON _ = fail "Not a HeartBeatRequest"

instance Arbitrary HeartBeatRequest where
    arbitrary =
        HeartBeatRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

fromHeartBeatRequest :: HeartBeatRequest -> IO (Word32, HeartBeat)
fromHeartBeatRequest (HeartBeatRequest addr uid intv hbid) =
    (,) <$> pure hbid <*> (HeartBeat (MB.Address addr) uid intv Nothing <$> newEmptyMVar)

toHeartBeatRequest :: (Word32, HeartBeat) -> HeartBeatRequest
toHeartBeatRequest (hbid, hb) =
    HeartBeatRequest (MB.unAddress $ hbAddress hb) (hbUid hb) (hbInterval hb) hbid