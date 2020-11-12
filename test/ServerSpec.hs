{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (serverSpec) where

import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.Hspec (shouldSatisfy, shouldThrow, Spec, around, describe, hspec, it, runIO, shouldBe)
import Types.Server (ConnectionInfo (..), ConnectionRequest (..), HeartBeatRequest, InitRequest, KeepAliveResponse, KeepAliveServ (..), OS)

import Modbus (ByteOrder (..), ModbusProtocol (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Server (ServerAPI, getInitState, server)

import Servant (Proxy (..), (:<|>) (..))
import System.Process (cleanupProcess, createProcess, proc)
import Types
    (serializeModData, bitsFromString, createModData
    , ReadWrite (..)
    , ModValue (..)
    , ModDataUpdate(..)
    , RegType (..)
    , setMDUModValue
    , KeepAliveResponse (..)
    )
import Control.Monad (void)
import Control.Exception.Safe (bracket)
import Data.Either (isLeft)
import Test.QuickCheck (arbitrary, generate)
import qualified Data.Text as T

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

-- Spin up the test TCP modbus client and make sure it is cleaned up after testing
businessLogic :: IO ()
businessLogic =
    bracket (createProcess (proc "./testserver" ["regs.csv"])) cleanupProcess (\_ -> hspec businessLogicSpec)

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
            let connectRequest =
                    ConnectionRequest
                        (TCPConnectionInfo (read "127.0.0.1") 5502 10)
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

            describe "GET /connectInfo" $ do
                let info = TCPConnectionInfo
                        (read "127.0.0.1")
                        5502
                        10
                it "returns Nothing when not connected" $ \port -> do
                    result <- runClientM getConnectionInfo (clientEnv port)
                    result `shouldBe` Right Nothing

                it "returns valid info when connected" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM getConnectionInfo (clientEnv port)
                    result `shouldBe` Right (Just info)

            describe "POST /disconnect" $ do

                it "returns error when not connected" $ \port -> do
                    result <- runClientM (disconnect "disconnect")  (clientEnv port)
                    result `shouldSatisfy` isLeft

                it "correctly disconnects" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM (disconnect "disconnect")  (clientEnv port)
                    result `shouldBe` Right ()

            describe "POST /parseModData" $ do

                mds <- runIO $ generate arbitrary
                invalidmds <- runIO $ generate arbitrary

                it "correctly parses valid ModData" $ \port -> do
                    let s = serializeModData mds
                    result <- runClientM (parseAndSend $ T.unpack s)  (clientEnv port)
                    result `shouldBe` Right mds

                it "fails on invalid input" $ \port -> do
                    result <- runClientM (parseAndSend invalidmds)  (clientEnv port)
                    result `shouldSatisfy` isLeft

            describe "POST /keepAlive" $ do

                kas <- runIO $ generate arbitrary
                it "returns valid keep alive response" $ \port -> do
                    void $ runClientM (connect connectRequest) (clientEnv port)
                    result <- runClientM (keepAlive kas)  (clientEnv port)
                    if flag kas
                    then result `shouldBe` Right KeepAliveActivated
                    else result `shouldBe` Right KeepAliveDisactivated

            describe "POST /byteorder" $ do

                bo <- runIO $ generate arbitrary
                it "returns valid byteorder" $ \port -> do
                    result <- runClientM (byteOrder bo)  (clientEnv port)
                    result `shouldBe` Right bo




