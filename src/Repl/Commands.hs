{-# LANGUAGE RankNTypes #-}
module Repl.Commands
    ( commandsCompl
    , getCommand
    , cmd
    , list
    ) where

import           Control.Concurrent               (killThread, tryReadMVar)
import           Control.Exception.Safe           (try)
import           Control.Monad.Trans              (lift, liftIO)
import           Control.Monad.Trans.Except       (ExceptT, except, runExceptT)
import           Control.Monad.Trans.State.Strict (get, put)
import           Data.Data                        (Proxy (..))
import           Data.Either.Combinators          (mapLeft)
import           Data.List                        (delete, find, uncons)
import           Data.Maybe                       (catMaybes, fromJust)
import           Data.Word                        (Word16, Word8)


import           CsvParser                        (parseCSVFile,
                                                   serializeCSVFile)
import           PrettyPrint                      (ppError, ppMultModData,
                                                   ppMultThreadState,
                                                   ppPlaceholderModData,
                                                   ppStrWarning, ppThreadError,
                                                   ppUid)
import           Repl.HelpFun                     (findModByName, getModByName,
                                                   getModByPair, getPairs,
                                                   invalidCmd)
import           Repl.Parser                      (pReplAddrNum, pReplArg,
                                                   pReplFloat, pReplInt,
                                                   pReplWord)
import           Types

getCommand :: String -> Command
getCommand s =
    case s of
        "readInputRegistersWord"    -> ReadInputRegistersWord
        "readInputRegistersFloat"   -> ReadInputRegistersFloat
        "readHoldingRegistersFloat" -> ReadHoldingRegistersFloat
        "readHoldingRegistersWord"  -> ReadHoldingRegistersWord
        "writeRegistersWord"        -> WriteRegistersWord
        "writeRegistersFloat"       -> WriteRegistersFloat
        "read"                      -> Read
        "write"                     -> Write
        "heartbeat"                 -> Heartbeat
        "stopHeartbeat"             -> StopHeartbeat
        "listHeartbeat"             -> ListHeartbeat
        "import"                    -> Import
        "export"                    -> Export
        "id"                        -> Id
        _                           -> CommandNotFound

-- Top level command function
cmd :: String -> Repl ()
cmd = runReplCommand

-- Top level command function
runReplCommand :: String -> Repl ()
runReplCommand input =
  let
    (str, args) = (fromJust . uncons . words) input
  in case getCommand str of
        ReadInputRegistersWord -> readRegisters args InputRegister (ModWord Nothing)
        ReadInputRegistersFloat -> readRegisters args InputRegister (ModFloat Nothing)
        ReadHoldingRegistersWord -> readRegisters args HoldingRegister (ModWord Nothing)
        ReadHoldingRegistersFloat -> readRegisters args HoldingRegister (ModFloat Nothing)
        WriteRegistersWord -> writeRegisters args HoldingRegister (ModWord Nothing)
        WriteRegistersFloat -> writeRegisters args HoldingRegister (ModFloat Nothing)
        Read -> readModData args
        Write -> writeModData args
        Heartbeat -> heartbeat args
        StopHeartbeat -> stopHeartbeat args
        ListHeartbeat -> listHeartbeat args
        Import -> replImport args
        Export -> replExport args
        Id -> replId args
        CommandNotFound -> liftIO $ putStrLn ("command not found: " ++ str)

commandsCompl :: [String]
commandsCompl = [
      "readInputRegistersWord"
    , "readInputRegistersFloat"
    , "readHoldingRegistersWord"
    , "readHoldingRegistersFloat"
    , "writeRegistersWord"
    , "writeRegistersFloat"
    , "read"
    , "write"
    , "heartbeat"
    , "stopHeartbeat"
    , "listHeartbeat"
    , "import"
    , "export"
    , "id"
    ]

-- list availiable commands
list :: a -> Repl ()
list _ = liftIO $ do
        putStrLn "List of commands:"
        putStrLn ""
        mapM_ putStrLn commandsCompl
        putStrLn ""
        putStrLn "For help type \":help [command]\""
        putStrLn ""

------------------------------------------------------------------------------------------
-- Read Registers
------------------------------------------------------------------------------------------

readRegisters :: [String] -> RegType -> ModValue-> Repl ()
readRegisters [address, number] rt mv = do
    state <- replGet
    let uid = replUId state
    response <- replReadRegisters address number rt mv uid
    liftIO $ ppPlaceholderModData response
readRegisters _ _ _ = invalidCmd

-- Parses the address and number of registers strings and
-- invokes the correct modbus read function
replReadRegisters :: String     -- Address
                  -> String     -- Number of registers
                  -> RegType    -- Register Type
                  -> ModValue   -- Word16 per value multiplier
                  -> Word8      -- Unit Id
                  -> Repl [ModData]
replReadRegisters addrStr numStr regType mv uid = do
    let moddata = do
            (addr, num) <- pReplAddrNum addrStr numStr
            return $
                    map
                    (\address -> createModData regType address mv uid)
                    $ getAddresses (addr,num) (getModValueMult mv)
    md <- replRunExceptT (except moddata) []
    replReadModData md


getAddresses :: (Word16, Word16) -> Word16 -> [Word16]
getAddresses (_ , 0) _ = []
getAddresses (start, num) mult =
    start : getAddresses (start + mult, num  - 1) mult

readModData :: [String] -> Repl ()
readModData [] = invalidCmd
readModData args = do
    mdata <- replModData <$> lift get
    let wrapped = except $ mapM (getModByName mdata) args
    mds <- replRunExceptT wrapped []
    response <- replReadModData mds
    liftIO $ ppMultModData response

replReadModData :: [ModData] -> Repl [ModData]
replReadModData mds = do
    state <- replGet
    let protocol = replProtocol state
    tpu <- replGetTPU
    let order = replByteOrder state
    let proxy = Proxy :: Proxy Client
    let sessions = map (readMBRegister proxy protocol tpu order) mds
    let worker = replBatchWorker state
    let client = replClient state
    maybemdata <- runReplClient $ mapM (runClient worker client) sessions
    mdata' <- replRunExceptT (except maybemdata) []
    return $ catMaybes mdata'

------------------------------------------------------------------------------------------
-- Write Registers
------------------------------------------------------------------------------------------

writeRegisters :: [String] -> RegType -> ModValue-> Repl ()
writeRegisters args rt mv = do
    let mPairs = getPairs args
    case mPairs of
        Nothing -> invalidCmd
        Just pairs -> do
            state <- replGet
            let uid = replUId state
            let addrValuePairs = map (getAddressModValue mv) pairs
            addrValuePairs' <- replRunExceptT (mapM except addrValuePairs) []
            let mds = map (\(addr, value) -> createModData rt addr value uid) addrValuePairs'
            replWriteModData mds

getAddressModValue :: ModValue -> (String, String) -> Either AppError (Word16, ModValue)
getAddressModValue  mv (addr, value) = do
    address <- pReplWord addr
    modvalue <- case mv of
            ModWord _  -> ModWord . Just <$> pReplWord value
            ModFloat _ -> ModFloat . Just <$> pReplFloat value
    return (address, modvalue)

writeModData :: [String] -> Repl ()
writeModData [] = invalidCmd
writeModData args = do
    mdata <- replModData <$> lift get
    let mPairs = getPairs args
    case mPairs of
        Nothing -> invalidCmd
        Just pairs -> do
            let wrapped = except $ getModByPair pairs mdata
            mds <- replRunExceptT wrapped []
            replWriteModData mds

-- Write a ModData to the server
-- Return the number of ModData written
replWriteModData :: [ModData] -> Repl ()
replWriteModData mds = do
    state <- replGet
    let protocol = replProtocol state
    tpu <- replGetTPU
    let order = replByteOrder state
    let proxy = Proxy :: Proxy Client
    let sessions = map (writeMBRegister proxy protocol tpu order) mds
    let worker = replBatchWorker state
    let client = replClient state
    maybemdata <- runReplClient $ mapM (runClient worker client) sessions
    replRunExceptT (except maybemdata) []
    return ()

------------------------------------------------------------------------------------------
-- HeartBeat
------------------------------------------------------------------------------------------

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
            addrInterval <- mapM replGetAddrInterval pairs
            mapM_ startHeartbeat addrInterval
  where replGetAddrInterval (s,t) = (,) <$> replGetAddr s <*> replGetInterval t

-- Setup the connection info for the server and call the spawn function
-- for every heartbeat
startHeartbeat :: (Word16, Int) -> Repl ()
startHeartbeat (addr, timer)
    | timer <= 0 = return ()
    | otherwise =
        afterActiveThreadCheck addr $ do
            state <- replGet
            let worker = replDirectWorker state
            let client = replClient state
            let uid = replUId state
            let tid = replTransactionId state
            let address = Address addr
            let protocol = replProtocol state
            heart <- liftIO $ heartBeatSignal protocol timer worker client uid tid address
            putHeartBeat heart

-- Parse the argument list and call stopHeartbeatThread on
-- each valid identifier
stopHeartbeat :: [String] -> Repl ()
stopHeartbeat [] = undefined
stopHeartbeat args = do
    checkActiveHeartbeat
    addrs <- mapM replGetAddr args
    mapM_ stopHeartbeatThread addrs


stopHeartbeatThread :: Word16 -> Repl ()
stopHeartbeatThread addr = do
    checkActiveHeartbeat
    state <- lift get
    let mthread = find (\x -> hbAddress x == Address addr) $ replPool state
    case mthread of
        Nothing -> liftIO $
            ppStrWarning $ "The heartbeat signal at address "
            ++ show addr
            ++ " was not found in the signal list.\n"
            ++ "Most probably it encoutered an error and has been terminated"
        Just thread' -> do
            liftIO $ killThread $ hbThreadId thread'
            removeThread thread'
            liftIO $ putStrLn $
                "The heartbeat signal at address "
                ++ show addr
                ++ " has been stopped."

-- removes the provided thead from the pool
removeThread :: HeartBeat -> Repl ()
removeThread ts = do
    state <- lift get
    let pool = delete ts $ replPool state
    lift $ put $ state { replPool = pool }

-- Put the provided heartbeat in the pool
putHeartBeat :: HeartBeat -> Repl ()
putHeartBeat st = do
    state <- replGet
    let pool = replPool state
    replPut $ state { replPool = st:pool }

-- Checks if the provided address is a ModData name
-- or a Word16 address
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

replGetInterval :: String -> Repl Int
replGetInterval s =
    replRunExceptT timer 0
  where
    timer = except $ pReplInt s

-- Check if there is an active heartbeat signal running in the address
-- and perform the provided action if not
afterActiveThreadCheck :: Word16 -> Repl () -> Repl ()
afterActiveThreadCheck addr action = do
    checkActiveHeartbeat
    state <- lift get
    let threads = replPool state
    let active = find (\x -> hbAddress x == Address addr) threads
    case active of
        Just _ -> liftIO $ ppStrWarning $
                    "A heartbeat signal is already running at address " ++ show addr
        Nothing -> action

-- Check all current running signals if they are
-- still active
checkActiveHeartbeat :: Repl ()
checkActiveHeartbeat = do
    state <- lift get
    let pool = replPool state
    active <- liftIO $ checkThreads pool
    lift $ put $ state { replPool = active }

-- Checks if the heartbeat threads are still running.
-- If some exception is raised in a hearbeat, it prints an error message and
-- removes the hearbeat from the pool
checkThreads :: [HeartBeat] -> IO [HeartBeat]
checkThreads [] = return []
checkThreads (x:xs) = do
    checked <- tryReadMVar (hbStatus x)
    case checked of
        Nothing -> (:) <$> return x <*> checkThreads xs
        Just err -> do
            ppThreadError x err
            checkThreads xs

-- List all active heartbeat signals
listHeartbeat :: [String] -> Repl ()
listHeartbeat [] = do
    checkActiveHeartbeat
    pool <- replPool <$> lift get
    if null pool
    then liftIO $ ppStrWarning "No active heartbeat signals"
    else liftIO $ ppMultThreadState pool
listHeartbeat _ = undefined

------------------------------------------------------------------------------------------
-- Import / Export
------------------------------------------------------------------------------------------

replImport :: [String] -> Repl ()
replImport [] = liftIO $ do
    putStrLn "Usage: import path-to-csv-file"
    putStrLn "e.g. import ~/path/to/file.csv"
    putStrLn "Type \":help import\" for more information"
replImport [filename] = do
    contents <- liftIO $ parseCSVFile filename
    mdata <- replRunExceptT (except contents) []
    state <- lift get
    lift $ put state {replModData = mdata}
    liftIO $ putStrLn $ show (length mdata) ++ " registers updated"
replImport _ = invalidCmd

replExport :: [String] -> Repl ()
replExport [] = liftIO $ do
    putStrLn "Usage: export path-to-csv-file"
    putStrLn "e.g. export ~/path/to/file.csv"
    putStrLn "Type \":help export\" for more information"
replExport [filename] = do
    state <- replGet
    let mdata = replModData state
    mdata' <- replReadModData mdata
    liftIO $ serializeCSVFile filename mdata'
    return ()
replExport _ = invalidCmd

------------------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------------------

-- Runs an ExceptT, returning a default value in case of AppError
replRunExceptT :: ExceptT AppError IO a -> a -> Repl a
replRunExceptT ex rt = do
    unwrapped <- liftIO $ runExceptT ex
    case unwrapped of
        Left err -> liftIO $ ppError err >> return rt
        Right x  -> return x

runReplClient :: IO a -> Repl (Either AppError a)
runReplClient action = liftIO $ mapLeft AppModbusError <$> try action

replGet :: Repl ReplState
replGet = lift get

replPut :: ReplState -> Repl ()
replPut  = lift . put

replGetTPU :: Repl TPU
replGetTPU = do
    state <- replGet
    let uid = replUId state
    tid <- liftIO $ getNewTID $ replTransactionId state
    return $ setTPU uid tid

replId :: [String] -> Repl ()
replId [] = do
    uid <- replUId <$> lift get
    liftIO $ ppUid uid
replId [uid] = do
    let uid' = pReplWord uid
    state <- lift get
    case uid' of
        Left err -> liftIO $ ppError err
        Right newid -> do
                lift $ put (state {replUId = newid})
                liftIO $ ppUid newid
replId _ = invalidCmd

