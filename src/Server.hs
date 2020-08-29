{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

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

runServer :: IO ()
runServer = do
    putStrLn "http://localhost:4000"
    scotty 4000 app

app :: ScottyM ()
app = do
    get "/" showLandingPage
    get "/app.js" $ file "frontend/app.js"
    get "/style.css" $ file "frontend/style.css"
    post "/register" register

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