{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer, server, getInitState, ServerAPI) where

import Control.Concurrent.STM (
    TVar,
    newTVarIO,
    readTVarIO,
    writeTVar,
 )
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO (liftIO))
import Network.Wai.Handler.Warp (run)
import Servant

import qualified Data.Text as T
import qualified Network.Socket as S
import qualified System.Process as PS

import Control.Concurrent (killThread, tryTakeMVar)
import Control.Exception (try)
import Control.Exception.Safe (SomeException)
import Control.Monad (foldM, void)
import CsvParser (runpCSV)
import Data.List (delete)
import Data.Word (Word32)
import Modbus (
    WordOrder,
    HeartBeat,
    ModbusClient (..),
    ModbusProtocol (..),
    RTUSession,
    SerialSettings (..),
    TCPSession,
    TID,
    getNewTID,
    getRTUClient,
    getRTUSerialPort,
    getTCPClient,
    getTCPSocket,
    hbStatus,
    hbThreadId,
    heartBeatSignal,
    initTID,
    rtuReadMBRegister,
    rtuUpdateMBRegister,
    tcpReadMBRegister,
    tcpUpdateMBRegister,
    unSR,
 )
import Network.Socket.KeepAlive
import qualified System.Hardware.Serialport as SP
import Types.ModData
import Types.Server

---------------------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------------------

type ServerAPI =
    "modData" :> ReqBody '[JSON] [ModDataUpdate] :> Post '[JSON] [ModDataUpdate]
        :<|> "connect" :> ReqBody '[JSON] ConnectionRequest :> Post '[JSON] ()
        :<|> "connectInfo" :> Get '[JSON] (Maybe ConnectionInfo)
        :<|> "disconnect" :> ReqBody '[JSON] String :> Post '[JSON] ()
        :<|> "parseModData" :> ReqBody '[JSON] String :> Post '[JSON] [ModData]
        :<|> "keepAlive" :> ReqBody '[JSON] KeepAliveServ :> Post '[JSON] KeepAliveResponse
        :<|> "wordOrder" :> ReqBody '[JSON] WordOrder :> Post '[JSON] WordOrder
        :<|> "startHeartbeat" :> ReqBody '[JSON] HeartBeatRequest :> Post '[JSON] [Word32]
        :<|> "stopHeartbeat" :> ReqBody '[JSON] [Word32] :> Post '[JSON] [Word32]
        :<|> "initHeartbeat" :> Post '[JSON] [HeartBeatRequest]
        :<|> "init" :> Get '[JSON] InitRequest
        :<|> Raw --FIX ME :: fix for path traversal attacks

serverAPI :: TVar ServState -> Server ServerAPI
serverAPI state =
    updateModData state
        :<|> connect state
        :<|> getConnectionInfo state
        :<|> disconnect state
        :<|> parseAndSend
        :<|> keepAlive state
        :<|> wordOrder state
        :<|> startHeartbeat state
        :<|> stopHeartbeat state
        :<|> initHeartbeat state
        :<|> initRequest state
        :<|> serveDirectoryWebApp "frontend"

proxyAPI :: Proxy ServerAPI
proxyAPI = Proxy

---------------------------------------------------------------------------------------------------------------
-- Server
---------------------------------------------------------------------------------------------------------------

server :: TVar ServState -> Application
server state = serve proxyAPI $ serverAPI state

runServer ::
    ModbusProtocol ->
    WordOrder -> -- Word order
    IO ()
runServer protocol order = do
    initState <- getInitState protocol order
    putStrLn "Modbus Client"
    putStrLn "Opening default browser"
    spawnBrowser 
    putStrLn "Starting server"
    run 4000 $ server initState

spawnBrowser :: IO ()
spawnBrowser =
    case getOs of
        Linux -> PS.callCommand "xdg-open http://localhost:4000/index.html"
        Windows -> PS.callCommand "start http://localhost:4000/index.html" 
        Other ->
            putStrLn $
                "Cannot start default browser\n"
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
        -- Do nothing if already connected
        TCPConnection{} -> throwError err400
        RTUConnection{} -> throwError err400
        NotConnected -> case connectionInfo of
            -- TCP
            TCPConnectionInfo ip portNum tm -> do
                let tms = tm * 1000000 -- in microseconds
                mSocket <- liftIO $ getTCPSocket ip portNum tms

                -- check socket
                case mSocket of
                    Nothing -> throwError err500
                    Just socket -> do
                        -- get client
                        client <- liftIO $ getTCPClient socket tms

                        -- update state
                        putState state $
                            state'
                                { servConnection = TCPConnection socket connectionInfo $ setActors client
                                , servProtocol = ModBusTCP
                                }
                        void $ keepAlive state kaValue
            -- RTU
            RTUConnectionInfo serial settings -> do
                let tms = SP.timeout (unSR settings) * 1000000 -- in microseconds
                mPort <- liftIO $ getRTUSerialPort serial settings

                -- check port
                case mPort of
                    Nothing -> throwError err500
                    Just port -> do
                        -- get client
                        client <- liftIO $ getRTUClient port tms

                        -- update state
                        putState state $
                            state'
                                { servConnection = RTUConnection port connectionInfo $ setActors client
                                , servProtocol = ModBusRTU
                                }

getConnectionInfo :: TVar ServState -> Handler (Maybe ConnectionInfo)
getConnectionInfo state = do
    connection <- servConnection <$> liftIO (readTVarIO state)
    case connection of
        TCPConnection _ info _ -> return $ Just info
        RTUConnection _ info _ -> return $ Just info
        NotConnected -> return Nothing

disconnect :: TVar ServState -> String -> Handler ()
disconnect state str = case str of
    "disconnect" -> do
        currentState <- liftIO $ readTVarIO state
        let connection = servConnection currentState
        case connection of
            TCPConnection socket _ _ -> disconnectTCP state socket
            RTUConnection port _ _ -> disconnectRTU state port
            NotConnected -> throwError err400
        liftIO $
            atomically $
                writeTVar state $
                    currentState
                        { servConnection = NotConnected
                        , servPool = []
                        }
    _ -> throwError err400

disconnectTCP :: TVar ServState -> S.Socket -> Handler ()
disconnectTCP state socket = do
    result <- liftIO $ try $ liftIO $ S.gracefulClose socket 1000
    handleDisconnect state result

disconnectRTU :: TVar ServState -> SP.SerialPort -> Handler ()
disconnectRTU state port = do
    result <- liftIO $ try $ SP.closeSerial port
    handleDisconnect state result

-- Handles a disconnect attempt and sets the server state accordingly
handleDisconnect :: TVar ServState -> Either SomeException () -> Handler ()
handleDisconnect state result =
    case result of
        Left (_ :: SomeException) -> do
            -- make sure we set the state to not connected
            currentState <- liftIO $ readTVarIO state
            liftIO $
                atomically $
                    writeTVar state $
                        currentState
                            { servConnection = NotConnected
                            , servPool = []
                            }
            throwError err500
        Right () -> return ()

-- Creates the server's initial state
getInitState :: ModbusProtocol -> WordOrder -> IO (TVar ServState)
getInitState protocol order = do
    tid <- initTID
    newTVarIO $
        ServState
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
        Just actors' ->
            let client = sclClient actors'
             in case servProtocol state' of
                    ModBusTCP ->
                        let worker = sclTCPBatchWorker actors'
                            sessions = traverse (tcpModUpdateSession tid order) mdus
                         in liftIO $ tcpRunClient worker client sessions
                    ModBusRTU ->
                        let worker = sclRTUBatchWorker actors'
                            sessions = traverse (rtuModUpdateSession order) mdus
                         in liftIO $ rtuRunClient worker client sessions

tcpModUpdateSession :: TID -> WordOrder -> ModDataUpdate -> TCPSession IO ModDataUpdate
tcpModUpdateSession tid order mdu =
    if mduSelected mdu
        then case mduRW mdu of
            MDURead -> tcpReadMBRegister tid order mdu
            MDUWrite -> tcpUpdateMBRegister tid order mdu
        else pure mdu

rtuModUpdateSession :: WordOrder -> ModDataUpdate -> RTUSession IO ModDataUpdate
rtuModUpdateSession order mdu =
    if mduSelected mdu
        then case mduRW mdu of
            MDURead -> rtuReadMBRegister order mdu
            MDUWrite -> rtuUpdateMBRegister order mdu
        else pure mdu

parseAndSend :: String -> Handler [ModData]
parseAndSend content =
    let md = runpCSV $ T.pack content
     in case md of
            Left _ -> throwError err400
            Right mds -> do
                pure mds

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
    rlt <- liftIO $
        S.withFdSocket sock $ \fd ->
            setKeepAlive fd ka
    case rlt of
        Left _ -> throwError err500
        Right () -> return ()

getKeepAliveServ :: S.Socket -> Handler Bool
getKeepAliveServ sock = liftIO $
    S.withFdSocket sock $ \fd -> getKeepAliveOnOff fd

wordOrder :: TVar ServState -> WordOrder -> Handler WordOrder
wordOrder state order = do
    currentState <- liftIO $ readTVarIO state
    liftIO $
        atomically $
            writeTVar state $
                currentState
                    { servOrd = order
                    }
    return order

-- Starts the provided heartbeat signal and returns the ids of all currently running
-- heartbeat signals
startHeartbeat :: TVar ServState -> HeartBeatRequest -> Handler [Word32]
startHeartbeat state hbr = do
    -- handle state
    currentState <- liftIO $ readTVarIO state
    let pool = servPool currentState
    let actors = getActors $ servConnection currentState
    let tid = servTID currentState

    -- Check active running signals
    runningPool <- liftIO $ checkServerPool pool

    -- get new heartbeat
    (hbid, hb) <- liftIO $ fromHeartBeatRequest hbr

    -- start heartbeat thread
    activated <- case actors of
        Nothing -> throwError err400
        Just actors' ->
            let client = sclClient actors'
             in case servProtocol currentState of
                    ModBusTCP ->
                        let worker = sclTCPBatchWorker actors'
                         in liftIO $ heartBeatSignal hb (Left worker) client tid
                    ModBusRTU ->
                        let worker = sclRTUBatchWorker actors'
                         in liftIO $ heartBeatSignal hb (Right worker) client tid

    -- update state
    let newPool = runningPool ++ [(hbid, activated)]
    liftIO $
        atomically $
            writeTVar state $
                currentState
                    { servPool = newPool
                    }

    pure $ fst <$> newPool

stopHeartbeat :: TVar ServState -> [Word32] -> Handler [Word32]
stopHeartbeat state ids = do
    -- manage state
    currentState <- liftIO $ readTVarIO state
    let pool = servPool currentState

    -- Check active running signals
    runningPool <- liftIO $ checkServerPool pool

    -- stop heartbeats
    newPool <- liftIO $ stopHeartBeatsById ids runningPool
    -- update state
    liftIO $
        atomically $
            writeTVar state $
                currentState
                    { servPool = newPool
                    }

    pure $ fst <$> newPool

stopHeartBeatsById :: [Word32] -> [(Word32, HeartBeat)] -> IO [(Word32, HeartBeat)]
stopHeartBeatsById ids pool =
    foldM stopHeartBeatProcess pool ids

-- find a heartbeat in a pool and stop its process if its active
stopHeartBeatProcess :: [(Word32, HeartBeat)] -> Word32 -> IO [(Word32, HeartBeat)]
stopHeartBeatProcess pool hbid = do
    let mpair = do
            hb <- lookup hbid pool
            thread <- hbThreadId hb
            pure (hb, thread)
    case mpair of
        Nothing -> pure pool
        Just (hb, thread) -> do
            killThread thread
            pure $ delete (hbid, hb) pool

-- Checks the pool of heartbeat signals to see if any has panicked
-- and remove them from the pool
checkServerPool :: [(Word32, HeartBeat)] -> IO [(Word32, HeartBeat)]
checkServerPool xs =
    foldM checkStatus [] (reverse xs)
  where
    checkStatus previous assoc@(_, hb) = do
        mvar <- tryTakeMVar $ hbStatus hb
        case mvar of
            Nothing -> pure $ assoc : previous
            Just _ -> pure previous

initHeartbeat :: TVar ServState -> Handler [HeartBeatRequest]
initHeartbeat state = do
    -- Check active running signals
    runningPool <- getServPool state

    pure $ toHeartBeatRequest <$> runningPool

initRequest :: TVar ServState -> Handler InitRequest
initRequest state =
    let os = getOs
    in InitRequest <$> getConnectionInfo state <*> pure os

getServPool :: TVar ServState -> Handler [(Word32, HeartBeat)]
getServPool state = do
    -- manage state
    currentState <- liftIO $ readTVarIO state
    let pool = servPool currentState

    -- Check active running signals
    liftIO $ checkServerPool pool
