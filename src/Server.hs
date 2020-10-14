{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Server (runServer)  where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM (writeTVar, readTVarIO, TVar, newTVar)
import Control.Monad.Trans (MonadIO(liftIO))
import Network.Wai.Handler.Warp (run)
import Servant

import qualified Data.Text as T
import qualified Network.Socket as S
import qualified System.OS as OS
import qualified System.Process as PS


import Control.Monad.Except (catchError)
import CsvParser (runpCSV)
import Control.Exception.Safe (SomeException)
import Control.Concurrent (killThread, threadDelay, ThreadId, forkIO, MVar)
import Control.Exception (try)
import qualified System.Hardware.Serialport as SP
import Modbus (keepAliveThread, runClient, updateMBRegister, readMBRegister, getNewTID, initTID, getRTUSerialPort, getTCPSocket, getRTUClient, getTCPClient, Worker, Client, Session, TID, ModbusProtocol (..), ByteOrder)
import Types.ModData
import Types.Server

---------------------------------------------------------------------------------------------------------------
-- API
---------------------------------------------------------------------------------------------------------------

type ServerAPI
    = "modData" :> ReqBody '[JSON] [ModDataUpdate] :> Post '[JSON] [ModDataUpdate]
    :<|> "connect" :> ReqBody '[JSON] ConnectionRequest :> Post '[JSON] ()
    :<|> "connectInfo" :> Get '[JSON] (Maybe ConnectionInfo)
    :<|> "disconnect" :> ReqBody '[JSON] String :> Post '[JSON] ()
    :<|> "parseModData" :> ReqBody '[JSON] String :> Post '[JSON] [ModData]
    :<|> "keepAlive" :> ReqBody '[JSON] KeepAlive :> Post '[JSON] ()
    :<|> Raw

serverAPI :: TVar ServState -> Server ServerAPI
serverAPI state
    = updateModData state
    :<|> connect state
    :<|> getConnectionInfo state
    :<|> disconnect state
    :<|> parseAndSend
    :<|> keepAlive state
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
    launchBrowser
    run 4000 $ server initState

launchBrowser :: IO ThreadId
launchBrowser = forkIO $ do
    threadDelay 1000000 -- wait for the server to start
    case OS.os of
        Nothing -> putStrLn "Operating system not detected"
        Just os -> catchError
            (spawnBrowser $ OS.unOS os)
            (const $ putStrLn $ "Error while starting default browser\n"
            ++ "To run the application, open http://localhost:4000/index.html in a browser"
            )


spawnBrowser :: String -> IO ()
spawnBrowser os
    | "Linux" `elem` osInfo
    = PS.spawnProcess "xdg-open" ["http://localhost:4000/index.html"] >> pure ()
    | "Windows" `elem` osInfo
    = PS.spawnProcess "start" ["http://localhost:4000/index.html"] >> pure ()
    | otherwise
    = putStrLn
        $ "Cannot start default browser\n"
        ++ "To run the application, open http://localhost:4000/index.html in a browser"
  where
      osInfo = words os


putState :: TVar ServState -> ServState -> Handler ()
putState oldState newState = liftIO $ atomically $ writeTVar oldState newState

---------------------------------------------------------------------------------------------------------------
-- Connections
---------------------------------------------------------------------------------------------------------------

connect :: TVar ServState -> ConnectionRequest -> Handler ()
connect state (ConnectionRequest connectionInfo kaValue) = do
    state' <- liftIO $ readTVarIO state
    case servConnection state' of
        TCPConnection {} -> throwError err400
        RTUConnection {} -> throwError err400
        NotConnected -> case connectionInfo of
            TCPConnectionInfo ip portNum tm -> do
                mSocket <- liftIO $ getTCPSocket ip portNum tm
                case mSocket of
                    Nothing -> throwError err300
                    Just socket -> do
                        client <- liftIO $ getTCPClient socket tm
                        putState state $ state'
                            {servConnection = TCPConnection socket connectionInfo $ getTCPActors client}
                        keepAlive state kaValue
            RTUConnectionInfo serial tm -> do
                mPort <- liftIO $ getRTUSerialPort serial tm
                case mPort of
                    Nothing -> throwError err300
                    Just port -> do
                        client <- liftIO $ getRTUClient port tm
                        putState state $ state'
                            {servConnection = RTUConnection port connectionInfo $ getRTUActors client}

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
                TCPConnection socket _ _ -> liftIO $ S.gracefulClose socket 1000
                RTUConnection port _ _ -> liftIO $ SP.closeSerial port
                NotConnected -> throwError err400
            liftIO $ atomically $ writeTVar state $ currentState
                { servConnection = NotConnected
                , servPool = []
                }
        _ -> throwError err400

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
        Nothing



---------------------------------------------------------------------------------------------------------------
-- Requests
---------------------------------------------------------------------------------------------------------------

keepAlive :: TVar ServState -> KeepAlive -> Handler ()
keepAlive state kaValue = do
    currentState <- liftIO $ readTVarIO state
    let actors = getActors $ servConnection currentState
    case servKeepAliveId currentState of
        Nothing ->
            case actors of
                Nothing -> throwError err400
                Just (ServerActors client _ _) -> do
                    thread <- liftIO $ forkIO $ keepAliveThread client $ 1000000 * interval kaValue
                    liftIO $ atomically $ writeTVar state currentState
                            { servKeepAliveId = Just thread
                            }
        Just thread ->
            liftIO $ killThread thread


updateModData :: TVar ServState -> [ModDataUpdate] -> Handler [ModDataUpdate]
updateModData state mdus = do
    state' <- liftIO $ readTVarIO state
    let prot = servProtocol state'
    let order = servOrd state'
    tid <- liftIO $ getNewTID $ servTID state'
    let actors = getActors $ servConnection state'
    case actors of
        Nothing -> throwError err400
        Just (ServerActors client _ batchWorker) -> do
            let sessions = map (modUpdateSession prot tid order) mdus
            resp <- liftIO $ mapM (runServerClientMaybe batchWorker client) sessions
            let resp' = liftException resp
            case resp' of
                Left _ -> throwError err500
                Right md' -> return $ mergeModDataUpdate md' mdus

modUpdateSession :: ModbusProtocol -> TID -> ByteOrder -> ModDataUpdate -> Maybe (Session IO ModDataUpdate)
modUpdateSession prot tid order mdu =
    if mduSelected mdu
    then
        case mduRW mdu of
            MDURead -> Just $ readMBRegister proxy prot tid order mdu
            MDUWrite -> Just $ updateMBRegister proxy prot tid order mdu
    else Nothing
  where
    proxy = Proxy :: Proxy Client

parseAndSend :: String -> Handler [ModData]
parseAndSend content =
    let
        md = runpCSV $ T.pack content
    in
        case md of
            Left _ -> throwError err400
            Right mds -> pure mds

runServerClientMaybe :: Worker IO -> MVar Client -> Maybe (Session IO a) -> IO ( Maybe (Either SomeException a))
runServerClientMaybe worker client (Just session) = do
    rlt <- try $ runClient worker client session
    return $ Just rlt
runServerClientMaybe _ _ Nothing = return Nothing

-- TODO : Find tail recursive alternative
liftException :: [Maybe (Either a b)] -> Either a [Maybe b]
liftException [] = Right []
liftException (x:xs) =
    case liftException xs of
        Left err -> Left err
        Right b -> case x of
            Nothing -> Right $ Nothing : b
            Just y -> case y of
                Left err -> Left err
                Right y' -> Right $ Just y' : b

mergeModDataUpdate :: [Maybe ModDataUpdate] -> [ModDataUpdate] -> [ModDataUpdate]
mergeModDataUpdate [] _ = []
mergeModDataUpdate _ [] = []
mergeModDataUpdate (x:xs) (y:ys) =
    case x of
        Nothing -> y:mergeModDataUpdate xs ys
        Just x' -> x':mergeModDataUpdate xs ys


