{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (serverSpec) where

import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.Hspec (Spec, around, describe, hspec, it, runIO, shouldBe, shouldSatisfy)
import Types.Server (ConnectionInfo (..), ConnectionRequest (..), HeartbeatRequest, InitRequest, KeepAliveResponse, KeepAliveServ (..), OS)
import TestHelper (pickFromList)
import Modbus (WordOrder (..), ModbusProtocol (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Server (ServerAPI, getInitState, server)

import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Servant (Proxy (..), (:<|>) (..))
import System.Process (cleanupProcess, createProcess, proc)
import Test.QuickCheck (arbitrary, generate)
import Types (OS(..), getOs, InitRequest (..), HeartbeatRequest (hbrId), KeepAliveResponse (..), ModDataUpdate (..), ModValue (..), ReadWrite (..), RegType (..), bitsFromString, createModData, serializeModData, setMDUModValue)
import Data.List ((\\))
import System.FilePath ((</>))
serverSpec :: IO ()
serverSpec = do
    hspec $ do
        roundtripSpecs (Proxy :: Proxy ConnectionInfo)
        roundtripSpecs (Proxy :: Proxy HeartbeatRequest)
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

-- Spin up the test TCP modbus client and make sure it is cleaned up after testing
businessLogic :: IO ()
businessLogic = do
    let os = getOs
    let testModbusServer = case os of
            Linux -> "testModbusServer" </> "testserver"
            Windows -> "testModbusServer" </> "testserver.exe"
            Other -> fail "Test server not availiable for this platform"
    bracket (createProcess (proc testModbusServer ["testModbusServer" </> "regs.csv"])) cleanupProcess (\_ -> hspec businessLogicSpec)

businessLogicSpec :: Spec
businessLogicSpec =
    -- `around` will start our Server before the tests and turn it off after
    around
        withUserApp
        $ do
            -- create a test client function
            let updateModData
                    :<|> connect
                    :<|> disconnect
                    :<|> parseAndSend
                    :<|> keepAlive
                    :<|> wordOrder
                    :<|> startHeartbeat
                    :<|> stopHeartbeat
                    :<|> initHeartbeat
                    :<|> initRequest
                    :<|> _ =
                        client (Proxy :: Proxy ServerAPI)
            -- create a servant-client ClientEnv
            baseUrl <- runIO $ parseBaseUrl "http://localhost"
            manager <- runIO $ newManager defaultManagerSettings
            let clientEnv port = mkClientEnv manager (baseUrl{baseUrlPort = port})
            -- testing scenarios start here
            let connInfo = TCPConnectionInfo (read "127.0.0.1") 5502 10
            let connectRequest =
                    ConnectionRequest
                        connInfo
                        $ KeepAliveServ False 20 10
            describe "POST /Connect" $
                it "should send the right response" $ \port -> do
                    result <- runClientM (connect connectRequest) (clientEnv port)
                    result `shouldBe` Right ()

            describe "POST /modData" $
                it "returns correct test server modData" $ \port -> do
                    let zeroMD = createModData InputRegister 0 (ModWord Nothing) 0
                    let mdu0 = MDU zeroMD True MDURead
                    let inputReg10 = createModData InputRegister 10 (ModWord Nothing) 0
                    let mdu10 = MDU inputReg10 True MDURead
                    let inputReg11 = createModData InputRegister 11 (ModWord Nothing) 0
                    let mdu11 = MDU inputReg11 True MDURead
                    let holdingReg15 = createModData HoldingRegister 15 (ModWord Nothing) 0
                    let mdu15 = MDU holdingReg15 True MDURead
                    -- write a holding register Float
                    let holdingReg16 = createModData HoldingRegister 16 (ModFloat (Just 3.14)) 0
                    let mdu16 = MDU holdingReg16 True MDUWrite
                    -- write a holding register Word
                    let holdingReg17 = createModData HoldingRegister 17 (ModWord (Just 777)) 0
                    let mdu17 = MDU holdingReg17 True MDUWrite
                    -- read a wordbit
                    let holdingReg18 = createModData HoldingRegister 18 (ModWordBit (bitsFromString "1001001100011100")) 0
                    let mdu18 = MDU holdingReg18 True MDUWrite
                    -- mdus
                    let requestMDU = [mdu0, mdu10, mdu11, mdu15, mdu16, mdu17, mdu18]
                    let responseMDU =
                            [ setMDUModValue mdu0 (ModWord (Just 0))
                            , setMDUModValue mdu10 (ModWord (Just 1500))
                            , setMDUModValue mdu11 (ModWord (Just 1501))
                            , setMDUModValue mdu15 (ModWord (Just 1700))
                            , mdu16
                            , mdu17
                            , mdu18
                            ]
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM (updateModData requestMDU) (clientEnv port)
                    result `shouldBe` Right responseMDU

            describe "POST /disconnect" $ do
                it "returns error when not connected" $ \port -> do
                    result <- runClientM (disconnect "disconnect") (clientEnv port)
                    result `shouldSatisfy` isLeft

                it "correctly disconnects" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM (disconnect "disconnect") (clientEnv port)
                    result `shouldBe` Right ()

            describe "POST /parseModData" $ do
                mds <- runIO $ generate arbitrary
                invalidmds <- runIO $ generate arbitrary

                it "correctly parses valid ModData" $ \port -> do
                    let s = serializeModData mds
                    result <- runClientM (parseAndSend $ T.unpack s) (clientEnv port)
                    result `shouldBe` Right (Right mds)

                it "fails on invalid input" $ \port -> do
                    result <- runClientM (parseAndSend invalidmds) (clientEnv port)
                    result `shouldSatisfy` isLeft

            describe "POST /keepAlive" $ do
                kas <- runIO $ generate arbitrary
                it "returns valid keep alive response" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM (keepAlive kas) (clientEnv port)
                    if flag kas
                        then result `shouldBe` Right KeepAliveActivated
                        else result `shouldBe` Right KeepAliveDisactivated

            describe "POST /wordorder" $ do
                bo <- runIO $ generate arbitrary

                it "returns valid wordorder" $ \port -> do
                    result <- runClientM (wordOrder bo) (clientEnv port)
                    result `shouldBe` Right bo

            describe "POST /startHeartbeat" $ do
                initHbs <- runIO $ generate arbitrary
                hbs <- runIO $ generate arbitrary

                it "returns running heartbeat signals" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    -- send some initial heartbeat requests
                    traverse_ (\hb -> runClientM (startHeartbeat hb) (clientEnv port)) initHbs
                    result <- runClientM (startHeartbeat hbs) (clientEnv port)
                    -- check that total heartbeat ids are returned
                    result `shouldBe` Right (map hbrId initHbs ++ [hbrId hbs])

            describe "POST /stopHeartbeat" $ do
                -- send some initial heartbeat requests
                initHbs <- runIO $ generate arbitrary
                let initHbsIds = map hbrId initHbs
                delHbs <- runIO $ pickFromList initHbs
                let delHbsIds = map hbrId delHbs
                let restIds = initHbsIds \\ delHbsIds

                it "returns running heartbeat signals" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    -- send some initial heartbeat requests
                    traverse_ (\hb -> runClientM (startHeartbeat hb) (clientEnv port)) initHbs
                    -- send stop to a subset of the requests
                    result <- runClientM (stopHeartbeat delHbsIds) (clientEnv port)
                    -- check that total heartbeat ids are returned
                    result `shouldBe` Right restIds

            describe "POST /initHeartbeat" $ do
                -- send some initial heartbeat requests
                initHbs <- runIO $ generate arbitrary

                it "returns running  (initial) heartbeat signals" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    -- send some initial heartbeat requests
                    traverse_ (\hb -> runClientM (startHeartbeat hb) (clientEnv port)) initHbs
                    result <- runClientM initHeartbeat (clientEnv port)
                    -- check that total heartbeat ids are returned
                    result `shouldBe` Right initHbs

            describe "GET /init" $ do
                let os = getOs

                it "returns valid initial configuration when connected" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM initRequest (clientEnv port)
                    -- check that total heartbeat ids are returned
                    result `shouldBe` Right (InitRequest (Just connInfo) os)

                it "returns valid initial configuration when not connected" $ \port -> do
                    result <- runClientM initRequest (clientEnv port)
                    -- check that total heartbeat ids are returned
                    result `shouldBe` Right (InitRequest Nothing os)