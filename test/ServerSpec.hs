module ServerSpec (serverSpec) where

import Data.Data (Proxy (..))
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.Hspec (hspec)
import Types.Server (OS, KeepAliveResponse, KeepAliveServ, InitRequest, ConnectionRequest, ConnectionInfo, HeartBeatRequest)

serverSpec :: IO ()
serverSpec = hspec $ do
    roundtripSpecs (Proxy :: Proxy ConnectionInfo)
    roundtripSpecs (Proxy :: Proxy HeartBeatRequest)
    roundtripSpecs (Proxy :: Proxy ConnectionRequest)
    roundtripSpecs (Proxy :: Proxy InitRequest)
    roundtripSpecs (Proxy :: Proxy KeepAliveServ)
    roundtripSpecs (Proxy :: Proxy KeepAliveResponse)
    roundtripSpecs (Proxy :: Proxy OS)
