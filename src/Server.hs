{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where

import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status
import Web.Scotty
    (ScottyM
    , scotty
    , get
    , post
    , ActionM
    , setHeader
    , file
    )

runServer :: IO ()
runServer = scotty 4000 app

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
register = undefined