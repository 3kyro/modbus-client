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

import qualified System.Modbus.TCP as MB

import Types (ConnectionData (..), ByteOrder, ServState (..), ModData (..))
import Modbus (modbusConnection, maybeConnect, getAddr)

type ServerAPI
    = "register" :> ReqBody '[JSON] [ModData] :> Post '[JSON] [ModData]
    :<|> "connect" :> ReqBody '[JSON] ConnectionData :> Post '[JSON] ()
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
    = reversem
    :<|> connect state
    :<|> serveDirectoryWebApp "frontend"

reversem :: [ModData] -> Handler [ModData]
reversem x = return (reverse x)

connect :: TVar ServState -> ConnectionData -> Handler ()
connect state (ConnectionData ip portNum tm) = do
    ServState _ order _ <- liftIO $ readTVarIO state
    mConn <- liftIO $ getMaybeConnection ip portNum tm
    case mConn of
        Nothing -> throwError err300
        Just c -> do
            liftIO $ atomically $ writeTVar state $ ServState (Just c) order []
            return ()

getServState :: IPv4 -> Int -> ByteOrder -> Int -> IO ServState
getServState ip portNum order tm = do
    conn <- getMaybeConnection ip portNum tm
    return $ ServState conn order []

getMaybeConnection :: IPv4 -> Int -> Int -> IO (Maybe MB.Connection)
getMaybeConnection ip portNum tm = do
    let sockAddr = getAddr ip portNum
    maybeSocket <- maybeConnect sockAddr tm
    return $ (`modbusConnection` tm) <$> maybeSocket



-- app :: Server ()
-- app = do
--     lift $ get "/" showLandingPage
--     lift $ get "/app.js" $ file "frontend/app.js"
--     lift $ get "/style.css" $ file "frontend/style.css"
--     lift $ post "/register" register
--     lift $ post "/connect" connect


-- connect :: ActionM ()
-- connect = do
--     ConnectionData ip port tm <- jsonData
--     liftIO $ print ip
--     liftIO $ print port
--     liftIO $ print tm
--     maybeSocket <- liftIO $ maybeConnect (getAddr ip port) tm
--     case maybeSocket of
--         Nothing -> status status500
--         Just _ -> status status200



-- showLandingPage :: ActionM ()
-- showLandingPage = do
--     setHeader "Content-Type" "text/html"
--     file "frontend/index.html"

-- register :: ActionM ()
-- register = do
--     got <- jsonData
--     let changed = map reverseName got
--     json changed
--     status status200
--   where
--       reverseName md = md { modName = reverse (modName md)}