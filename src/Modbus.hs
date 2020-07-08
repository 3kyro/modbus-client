module Modbus where

import System.Modbus.TCP
import Data.Word (Word16)
import qualified Network.Socket as S
import Network.Socket.ByteString (send, recv)
import Control.Monad.Except

getStatus :: Session [Word16]
getStatus = readInputRegisters 0 0 0 0 5

local :: S.Socket -> Connection
local s = Connection {
        connWrite = send s,
        connRead = recv s,
        connCommandTimeout = 1000,
        connRetryWhen = (\e n -> False)     
}


addr = S.SockAddrInet 5502 (S.tupleToHostAddress (127, 0, 0, 1))

try :: S.SockAddr -> IO ()
try address = do
    putStrLn ("Connecting to " ++ show address ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s address
    putStrLn "connected"
    a <- runExceptT $ runSession (local s) getStatus
    print a
