{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where


import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.Trans (liftIO, lift)
import Data.IP (IPv4)
import Network.HTTP.Types.Status
import Web.Scotty
    (ScottyM
    , scotty
    , get
    , post
    , ActionM
    , setHeader
    , file
    , jsonData
    , json
    , status
    )

import Types
import Modbus (modbusConnection, maybeConnect, getAddr)

runServerApp :: ServState -> Server a-> ScottyM a
runServerApp state serv = evalStateT serv state

runServer :: IPv4       -- Modbus Server IP address
          -> Int        -- Port number
          -> ByteOrder  -- Byte order
          -> Int        -- timeout
          -> IO ()
runServer ip portNum order tm = do
    let sockAddr = getAddr ip portNum
    maybeSocket <- maybeConnect sockAddr tm
    let conn = (`modbusConnection` tm) <$> maybeSocket
    let state = ServState conn order []
    putStrLn "http://localhost:4000"
    scotty 4000 $ runServerApp state app

app :: Server ()
app = do
    lift $ get "/" showLandingPage
    lift $ get "/app.js" $ file "frontend/app.js"
    lift $ get "/style.css" $ file "frontend/style.css"
    lift $ post "/register" register
    lift $ post "/connect" connect


connect :: ActionM ()
connect = do
    ConnectionData ip port tm <- jsonData
    liftIO $ print ip
    liftIO $ print port
    liftIO $ print tm
    maybeSocket <- liftIO $ maybeConnect (getAddr ip port) tm
    case maybeSocket of
        Nothing -> status status500
        Just _ -> status status200



showLandingPage :: ActionM ()
showLandingPage = do
    setHeader "Content-Type" "text/html"
    file "frontend/index.html"

register :: ActionM ()
register = do
    got <- jsonData
    let changed = map reverseName got
    json changed
    status status200
  where
      reverseName md = md { modName = reverse (modName md)}