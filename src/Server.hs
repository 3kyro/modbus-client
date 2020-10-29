{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Server (runServer)  where

import           Control.Concurrent.STM     (TVar, newTVar, readTVarIO,
                                             writeTVar)
import           Control.Monad.STM          (atomically)
import           Control.Monad.Trans        (MonadIO (liftIO))
import           Network.Wai.Handler.Warp   (run)
import           Servant

import qualified Data.Text                  as T
import qualified Network.Socket             as S
import qualified System.Info
import qualified System.Process             as PS


import           Control.Exception          (try)
import           Control.Exception.Safe     (SomeException)
import           CsvParser                  (runpCSV)
import           Modbus                     (ByteOrder, ModbusClient (..),
                                             ModbusProtocol (..), RTUSession,
                                             SerialSettings (..), TCPSession,
                                             TID, getNewTID, getRTUClient,
                                             getRTUSerialPort, getTCPClient,
                                             getTCPSocket, initTID,
                                             rtuReadMBRegister,
                                             rtuUpdateMBRegister,
                                             tcpReadMBRegister,
                                             tcpUpdateMBRegister, unSR)
import           Network.Socket.KeepAlive
import qualified System.Hardware.Serialport as SP
import           Types.ModData
import           Types.Server
---------------------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------------------

type ServerAPI
    = "modData" :> ReqBody '[JSON] [ModDataUpdate] :> Post '[JSON] [ModDataUpdate]
    :<|> "connect" :> ReqBody '[JSON] ConnectionRequest :> Post '[JSON] ()
    :<|> "connectInfo" :> Get '[JSON] (Maybe ConnectionInfo)
    :<|> "disconnect" :> ReqBody '[JSON] String :> Post '[JSON] ()
    :<|> "parseModData" :> ReqBody '[JSON] String :> Post '[JSON] [ModData]
    :<|> "keepAlive" :> ReqBody '[JSON] KeepAliveServ :> Post '[JSON] KeepAliveResponse
    :<|> "byteOrder" :> ReqBody '[JSON] ByteOrder :> Post '[JSON] ByteOrder
    :<|> "init" :> Get '[JSON] InitRequest
    :<|> Raw

serverAPI :: TVar ServState -> Server ServerAPI
serverAPI state
    = updateModData state
    :<|> connect state
    :<|> getConnectionInfo state
    :<|> disconnect state
    :<|> parseAndSend
    :<|> keepAlive state
    :<|> byteOrder state
    :<|> initRequest state
    :<|> serveDirectoryWebApp "frontend"

proxyAPI :: Proxy ServerAPI
proxyAPI = Proxy

---------------------------------------------------------------------------------------------------------------
-- Server
---------------------------------------------------------------------------------------------------------------

server :: TVar ServState -> Application
server state = serve proxyAPI $ serverAPI state

runServer :: ModbusProtocol
          -> ByteOrder  -- Byte order
          -> IO ()
runServer protocol order = do
    initState <- getInitState protocol order
    spawnBrowser System.Info.os
    run 4000 $ server initState

spawnBrowser :: String -> IO ()
spawnBrowser os =
    case os of
    "linux" -> PS.spawnProcess "xdg-open" ["http://localhost:4000/index.html"] >> pure ()
    "windows" -> PS.spawnProcess "start" ["http://localhost:4000/index.html"] >> pure ()
    _ -> putStrLn
        $ "Cannot start default browser\n"
        ++ "To run the application, open http://localhost:4000/index.html in a browser"


putState :: TVar ServState -> ServState -> Handler ()
putState oldState newState = liftIO $ atomically $ writeTVar oldState newState

---------------------------------------------------------------------------------------------------------------
-- Connections
---------------------------------------------------------------------------------------------------------------

connect :: TVar ServState -> ConnectionRequest -> Handler ()
connect state (ConnectionRequest connectionInfo kaValue) = do
    state' <- liftIO $ readTVarIO state
    case servConnection state' of

        -- Do nothing if alrady connected
        TCPConnection {} -> throwError err400
        RTUConnection {} -> throwError err400

        NotConnected -> case connectionInfo of

            TCPConnectionInfo ip portNum tm -> do
                let tms = tm * 1000000 -- in microseconds
                mSocket <- liftIO $ getTCPSocket ip portNum tms
                case mSocket of
                    Nothing -> throwError err500
                    Just socket -> do
                        client <- liftIO $ getTCPClient socket tms
                        putState state $ state'
                            { servConnection = TCPConnection socket connectionInfo $ setActors client
                            , servProtocol = ModBusTCP
                            }
                        keepAlive state kaValue
                        return ()

            RTUConnectionInfo serial settings -> do
                let tms = SP.timeout (unSR settings) * 1000000 -- in microseconds
                mPort <- liftIO $ getRTUSerialPort serial settings
                case mPort of
                    Nothing -> throwError err500
                    Just port -> do
                        client <- liftIO $ getRTUClient port tms
                        putState state $ state'
                            { servConnection = RTUConnection port connectionInfo $ setActors client
                            , servProtocol = ModBusRTU
                            }


getConnectionInfo :: TVar ServState -> Handler (Maybe ConnectionInfo)
getConnectionInfo state = do
        connection <- servConnection <$> liftIO (readTVarIO state)
        case connection of
            TCPConnection _ info _ -> return $ Just info
            RTUConnection _ info _ -> return $ Just info
            NotConnected           -> return Nothing

disconnect :: TVar ServState -> String -> Handler ()
disconnect state str = case str of
        "disconnect" -> do
            currentState <- liftIO $ readTVarIO state
            let connection = servConnection currentState
            case connection of
                TCPConnection socket _ _ -> disconnectTCP state socket
                RTUConnection port _ _   -> disconnectRTU state port
                NotConnected             -> throwError err400
            liftIO $ atomically $ writeTVar state $ currentState
                { servConnection = NotConnected
                , servPool = []
                }
        _ -> throwError err400

disconnectTCP :: TVar ServState -> S.Socket -> Handler ()
disconnectTCP state socket = do
    result <- liftIO $ try $ liftIO $ S.gracefulClose socket 1000
    case result of
        Left (_ :: SomeException) -> do
            -- make sure we set the state to not connected
            currentState <- liftIO $ readTVarIO state
            liftIO $ atomically $ writeTVar state $ currentState
                { servConnection = NotConnected
                , servPool = []
                }
            throwError err500
        Right () -> return ()

disconnectRTU :: TVar ServState -> SP.SerialPort -> Handler ()
disconnectRTU state port = do
    result <- liftIO $ try $ SP.closeSerial port
    case result of
        Left (_ :: SomeException) -> do
            -- make sure we set the state to not connected
            currentState <- liftIO $ readTVarIO state
            liftIO $ atomically $ writeTVar state $ currentState
                { servConnection = NotConnected
                , servPool = []
                }
            throwError err500
        Right () -> return ()

-- Creates the initial state the server boot's up with
getInitState :: ModbusProtocol -> ByteOrder -> IO (TVar ServState)
getInitState protocol order = do
    tid <- initTID
    atomically $ newTVar $ ServState
        NotConnected
        protocol
        order
        tid
        []

---------------------------------------------------------------------------------------------------------------
-- Requests
---------------------------------------------------------------------------------------------------------------

updateModData :: TVar ServState -> [ModDataUpdate] -> Handler [ModDataUpdate]
updateModData state mdus = do
    state' <- liftIO $ readTVarIO state
    let order = servOrd state'
    tid <- liftIO $ getNewTID $ servTID state'
    let actors = getActors $ servConnection state'
    case actors of
        Nothing -> throwError err400
        Just actors ->
            let client = sclClient actors
            in case servProtocol state' of
                ModBusTCP ->
                    let worker = sclTCPBatchWorker actors
                        sessions = traverse (tcpModUpdateSession tid order) mdus
                    in  liftIO $ tcpRunClient worker client sessions
                ModBusRTU ->
                    let worker = sclRTUBatchWorker actors
                        sessions = traverse (rtuModUpdateSession order) mdus
                    in  liftIO $ rtuRunClient worker client sessions

tcpModUpdateSession :: TID -> ByteOrder -> ModDataUpdate -> TCPSession IO ModDataUpdate
tcpModUpdateSession tid order mdu =
    if mduSelected mdu
    then
        case mduRW mdu of
            MDURead  -> tcpReadMBRegister tid order mdu
            MDUWrite -> tcpUpdateMBRegister tid order mdu
    else pure mdu

rtuModUpdateSession :: ByteOrder -> ModDataUpdate -> RTUSession IO ModDataUpdate
rtuModUpdateSession order mdu =
    if mduSelected mdu
    then
        case mduRW mdu of
            MDURead  -> rtuReadMBRegister order mdu
            MDUWrite -> rtuUpdateMBRegister order mdu
    else pure mdu

parseAndSend :: String -> Handler [ModData]
parseAndSend content =
    let
        md = runpCSV $ T.pack content
    in
        case md of
            Left _    -> throwError err400
            Right mds -> pure mds

keepAlive :: TVar ServState -> KeepAliveServ -> Handler KeepAliveResponse
keepAlive state kaValue = do
    currentState <- liftIO $ readTVarIO state
    let conn = servConnection currentState
    case conn of
        TCPConnection sock _ _ -> do
            setKeepAliveServ sock (toKeepAlive kaValue)
            on <- getKeepAliveServ sock
            if on
            then return KeepAliveActivated
            else return KeepAliveDisactivated
        _ -> throwError err400

setKeepAliveServ :: S.Socket -> KeepAlive -> Handler ()
setKeepAliveServ sock ka = do
    rlt <- liftIO $ S.withFdSocket sock $ \fd ->
        setKeepAlive fd ka
    case rlt of
        Left _   -> throwError err500
        Right () -> return ()

getKeepAliveServ :: S.Socket -> Handler Bool
getKeepAliveServ sock = liftIO $
    S.withFdSocket sock $ \fd -> getKeepAliveOnOff fd

byteOrder :: TVar ServState -> ByteOrder -> Handler ByteOrder
byteOrder state order = do
    currentState <- liftIO $ readTVarIO state
    liftIO $ atomically $ writeTVar state $ currentState
        { servOrd = order
        }
    return order

initRequest :: TVar ServState -> Handler InitRequest
initRequest state =
    let
    os = case System.Info.os of
        "linux"   -> pure Linux
        "windows" -> pure Windows
        _         -> pure Other
    in InitRequest <$> getConnectionInfo state <*> os
