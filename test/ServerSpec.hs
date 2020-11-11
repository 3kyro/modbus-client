module ServerSpec (serverSpec) where

import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.Hspec (Spec, around, describe, hspec, it, runIO, shouldBe)
import Types.Server (ConnectionInfo (..), ConnectionRequest (..), HeartBeatRequest, InitRequest, KeepAliveResponse, KeepAliveServ (..), OS)

import Modbus (ByteOrder (..), ModbusProtocol (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Server (ServerAPI, getInitState, server)

import Servant (Proxy (..), (:<|>) (..))
import System.Process (cleanupProcess, createProcess, proc)

serverSpec :: IO ()
serverSpec = do
    hspec $ do
        roundtripSpecs (Proxy :: Proxy ConnectionInfo)
        roundtripSpecs (Proxy :: Proxy HeartBeatRequest)
        roundtripSpecs (Proxy :: Proxy ConnectionRequest)
        roundtripSpecs (Proxy :: Proxy InitRequest)
        roundtripSpecs (Proxy :: Proxy KeepAliveServ)
        roundtripSpecs (Proxy :: Proxy KeepAliveResponse)
        roundtripSpecs (Proxy :: Proxy OS)
    businessLogic

withUserApp :: (Warp.Port -> IO ()) -> IO ()
-- testWithApplication makes sure the action is executed after the server has
-- started and is being properly shutdown.
withUserApp action = do
    initState <- getInitState ModBusTCP LE
    Warp.testWithApplication (pure $ server initState) action

businessLogic :: IO ()
businessLogic = do
    -- spin up a TCP server
    r <- createProcess (proc "./testserver" ["regs.csv"])
    -- run business logic tests
    hspec businessLogicSpec
    -- shut down the serve
    cleanupProcess r

businessLogicSpec :: Spec
businessLogicSpec =
    -- `around` will start our Server before the tests and turn it off after
    around
        withUserApp
        $ do
            -- create a test client function
            let updateModData
                    :<|> connect
                    :<|> getConnectionInfo
                    :<|> disconnect
                    :<|> parseAndSend
                    :<|> keepAlive
                    :<|> byteOrder
                    :<|> startHeartbeat
                    :<|> stopHeartbeat
                    :<|> initHeartbeat
                    :<|> initRequest
                    :<|> serverDirectoryWebApp =
                        client (Proxy :: Proxy ServerAPI)
            -- create a servant-client ClientEnv
            baseUrl <- runIO $ parseBaseUrl "http://localhost"
            manager <- runIO $ newManager defaultManagerSettings
            let clientEnv port = mkClientEnv manager (baseUrl{baseUrlPort = port})
            -- testing scenarios start here
            describe "POST /Connect" $ do
                let request =
                        ConnectionRequest
                            (TCPConnectionInfo (read "127.0.0.1") 5502 100)
                            $ KeepAliveServ False 20 10

                it "should send the right response" $ \port -> do
                    result <- runClientM (connect request) (clientEnv port)
                    result `shouldBe` Right ()

