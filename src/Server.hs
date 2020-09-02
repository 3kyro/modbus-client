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
import Data.IP (IPv4)
import Network.Wai.Handler.Warp (run)
import Servant

import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB

import Types (ConnectionData (..), ByteOrder, ServState (..), ModData (..))
import Modbus (modSession, modbusConnection, maybeConnect, getAddr)
import Control.Monad.Except (runExceptT)

type ServerAPI
    = "register" :> ReqBody '[JSON] [ModData] :> Post '[JSON] [ModData]
    :<|> "connect" :> ReqBody '[JSON] ConnectionData :> Post '[JSON] ()
    :<|> "connectInfo" :> Get '[JSON] (Maybe ConnectionData)
    :<|> "disconnect" :> ReqBody '[JSON] String :> Post '[JSON] ()
    :<|> Raw

proxyAPI :: Proxy ServerAPI
proxyAPI = Proxy

server :: TVar ServState -> Application
server state = serve proxyAPI $ serverAPI state

runServer :: IPv4       -- Modbus Server IP address
          -> Int        -- Port number
          -> ByteOrder  -- Byte order
          -> Int        -- timeout
          -> IO ()
runServer ip portNum order tm = do
    state <- getServState ip portNum order tm
    st <- atomically $ newTVar state
    run 4000 $ server st

serverAPI :: TVar ServState -> Server ServerAPI
serverAPI state
    = readModData state
    :<|> connect state
    :<|> getConnectionInfo state
    :<|> disconnect state
    :<|> serveDirectoryWebApp "frontend"

readModData :: TVar ServState -> [ModData] -> Handler [ModData]
readModData state md = do
    ServState conn order _ _ <- liftIO $ readTVarIO state
    case conn of
        Nothing -> throwError err400
        Just (_, mbc) -> do
            resp <- liftIO $ runExceptT $ MB.runSession mbc (modSession md order)
            case resp of
                Left _ -> throwError err500
                Right md' -> return md'

connect :: TVar ServState -> ConnectionData -> Handler ()
connect state dt@(ConnectionData ip portNum tm) = do
    ServState _ order _ _ <- liftIO $ readTVarIO state
    mConn <- liftIO $ getMaybeConnection ip portNum tm
    case mConn of
        Nothing -> throwError err300
        Just c -> do
            liftIO $ atomically $ writeTVar state $ ServState (Just c) order [] (Just dt)
            return ()

getConnectionInfo :: TVar ServState -> Handler (Maybe ConnectionData)
getConnectionInfo state = servConnInfo <$> liftIO (readTVarIO state)

disconnect :: TVar ServState -> String -> Handler ()
disconnect state s = case s of
        "disconnect" -> do
            mCon <- servConn <$> liftIO (readTVarIO state)
            case mCon of
                Nothing -> throwError err410
                Just (sock, _) -> do
                    liftIO $ S.gracefulClose sock 1000
                    return ()
        _ -> throwError err400

getServState :: IPv4 -> Int -> ByteOrder -> Int -> IO ServState
getServState ip portNum order tm = do
    conn <- getMaybeConnection ip portNum tm
    case conn of
        Nothing -> return $ ServState Nothing order [] Nothing
        Just _ -> return $ ServState conn order [] $ Just $ ConnectionData ip portNum tm

getMaybeConnection :: IPv4 -> Int -> Int -> IO (Maybe (S.Socket , MB.Connection))
getMaybeConnection ip portNum tm = do
    let sockAddr = getAddr ip portNum
    maybeSocket <- maybeConnect sockAddr tm
    return $ (,) <$> maybeSocket <*> maybeCon maybeSocket
  where
    maybeCon :: Maybe S.Socket -> Maybe MB.Connection
    maybeCon maybeSocket = (`modbusConnection` tm) <$> maybeSocket
