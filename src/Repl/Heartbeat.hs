module Repl.Heartbeat
    ( heartbeat
    , stopHeartbeat
    , listHeartbeat
    ) where

import Control.Concurrent (killThread, forkFinally, newEmptyMVar, tryReadMVar, threadDelay, putMVar, MVar)
import Control.Exception (SomeException(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.State.Strict (put, get)
import Data.List (find, delete)
import Data.Word (Word8, Word16)
import Network.Socket.ByteString (recv, send)

import qualified Network.Socket as S
import qualified System.Modbus.TCP as MB
import qualified System.Timeout as TM

import Repl.Error (replRunExceptT, runReplSession)
import Repl.Parser (pReplInt, pReplArg)
import PrettyPrint (ppStrError, ppThreadError, ppMultThreadState, ppStrWarning)
import Repl.HelpFun (invalidCmd, getPairs, findModByName)
import Types

-- Parse the input and call startHeartbeat with the
-- address and timer pairs
heartbeat :: [String] -> Repl ()
heartbeat [] = liftIO $ putStrLn $
    "Usage: heartbeat [identifier] [timer(ms)]\n"
    ++ "e.g. heartbeat 10 1000000 watch_reg 500000"
heartbeat args = do
    let mPairs = getPairs args
    case mPairs of
        Nothing -> invalidCmd
        Just pairs -> do
            addrTimer <- mapM replGetAddrTimer pairs
            mapM_ startHeartbeat addrTimer
  where replGetAddrTimer (s,t) = (,) <$> replGetAddr s <*> replGetTimer t

-- Setup the connection info for the server and call the spawn function
-- for every heartbeat
startHeartbeat :: (Word16, Int) -> Repl ()
startHeartbeat (addr, timer)
    | timer <= 0 = return ()
    | otherwise =
        afterActiveThreadCheck addr $ do
            uid <- replUId <$> lift get
            newV <- liftIO newEmptyMVar
            sock <- replSockAddr <$> replAsk
            tm <- replTimeout <$> replAsk
            thread <- liftIO $ forkFinally (spawn addr uid timer sock tm newV) (setThreadMVar newV)
            let threadS = ThreadState addr timer thread newV
            putThreadState threadS
            liftIO $ putStrLn $ "Watchdog launched at address: " ++ show addr ++ ", with an interval of " ++ show timer ++ "ms"

-- Check if there is an active heartbeat signal running in the address
-- and perform the provided action if not
afterActiveThreadCheck :: Word16 -> Repl () -> Repl ()
afterActiveThreadCheck addr action = do
    checkActiveHeartbeat
    state <- lift get
    let threads = replPool state
    let active = find (\x -> threadAddr x == addr) threads
    case active of
        Just _ -> liftIO $ ppStrWarning $
                    "A watchdog is already running at address " ++ show addr
        Nothing -> action


-- Start a heartbeat signal at the  given address. Heartbeat will start a new thread
-- that will increment the provided register each interval
spawn :: Word16             -- Address of holding register
      -> Word8              -- unit id
      -> Int                -- heartbeat period in ms
      -> S.SockAddr         -- server socket address
      -> Int                -- timeout
      -> MVar SomeException -- bookeeping MVar
      -> IO ()
spawn reg uid timer sock tm var = do
    ms <- connect sock tm
    case ms of
        Nothing -> ppStrError "Heartbeat connection timed out"
        Just s' -> go 0 (conn' s')
  where
    conn' s = MB.Connection (send s) (recv s) (tm * 1000) (const . const False)
    go acc mbConn = do
        let acc' = acc + 1
        threadDelay $ timer * 1000
        wrapped <- runExceptT $ runReplSession mbConn $ MB.writeSingleRegister 0 0 (MB.UnitId uid) (MB.RegAddress reg) acc'
        case wrapped of
            Left err -> putMVar var (SomeException err)
            Right _ -> go acc' mbConn

-- Connect to the server using a new socket and checking for a timeout
connect :: S.SockAddr -> Int -> IO (Maybe S.Socket)
connect addr tm = do
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    m <- TM.timeout tm (S.connect s addr)
    case m of
        Nothing -> return Nothing
        Just () -> return (Just s)

-- Parse the argument list and call stopHeartbeatThread on
-- each valid identifier
stopHeartbeat :: [String] -> Repl ()
stopHeartbeat [] = undefined
stopHeartbeat args = do
    checkActiveHeartbeat
    addrs <- mapM replGetAddr args
    mapM_ stopHeartbeatThread addrs

-- It would be great if we got an exception inside the Repl Monad
-- when one of the watchdog threads crashed, but this is tricky to do
-- (see A tale of two brackets : https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets/)
-- Instead we will be using an Mvar to check if the thread has crashed when we try to kill it
stopHeartbeatThread :: Word16 -> Repl ()
stopHeartbeatThread addr = do
    checkActiveHeartbeat
    state <- lift get
    let mthread = find (\x -> threadAddr x == addr) $ replPool state
    case mthread of
        Nothing -> liftIO $
            ppStrWarning $ "The heartbeat signal at address "
            ++ show addr
            ++ " was not found in the signal list.\n"
            ++ "Most probably it encoutered an error and has been terminated"
        Just thread' -> do
            liftIO $ killThread $ threadId thread'
            removeThread thread'
            liftIO $ putStrLn $
                "The heartbeat signal at address "
                ++ show addr
                ++ " has been stopped."

-- List all active heartbeat signals
listHeartbeat :: [String] -> Repl ()
listHeartbeat [] = do
    checkActiveHeartbeat
    pool <- replPool <$> lift get
    if null pool
    then liftIO $ ppStrWarning "No active watchdog timers"
    else liftIO $ ppMultThreadState pool
listHeartbeat _ = undefined

-- Check all current running signals if they are
-- still active
checkActiveHeartbeat :: Repl ()
checkActiveHeartbeat = do
    state <- lift get
    let pool = replPool state
    active <- liftIO $ checkThreads pool
    lift $ put $ state { replPool = active }

-- Checks if the provided thread and return true is still running.
-- If some exception is raised, it prints an error message and
-- removes the thread from the tread pool
checkThreads :: [ThreadState] -> IO [ThreadState]
checkThreads [] = return []
checkThreads (x:xs) = do
    checked <- tryReadMVar (threadMVar x)
    case checked of
        Nothing -> (:) <$> return x <*> checkThreads xs
        Just err -> do
            ppThreadError x err
            checkThreads xs

--
replGetAddr :: String -> Repl Word16
replGetAddr s = do
    mdata <- replModData <$> lift get
    let wrapped = do
            replArg <- pReplArg s
            case replArg of
                ReplName name -> do
                    md <- findModByName mdata name
                    return (modAddress md)
                ReplAddr addr -> return addr
    replRunExceptT (except wrapped) 0

replGetTimer :: String -> Repl Int
replGetTimer s =
    replRunExceptT timer 0
  where
    timer = except $ pReplInt s

setThreadMVar :: MVar SomeException -> Either SomeException () -> IO ()
setThreadMVar mv eitherExcept =
    case eitherExcept of
        Left exc -> putMVar mv exc
        Right () -> return ()

-- removes the provided thead from the pool
removeThread :: ThreadState -> Repl ()
removeThread ts = do
    state <- lift get
    let pool = delete ts $ replPool state
    lift $ put $ state { replPool = pool }

-- Put the provided thread in the pool
putThreadState :: ThreadState -> Repl ()
putThreadState st = do
    state <- lift get
    let pool = replPool state
    lift $ put $ state { replPool = st:pool }