{-# LANGUAGE RankNTypes #-}
module Repl.Commands
    ( commandsCompl
    , getCommand
    , cmd
    , list
    ) where

import           Control.Monad.Except             (runExceptT)
import           Control.Monad.Trans              (lift, liftIO)
import           Control.Monad.Trans.Except       (except)
import           Control.Monad.Trans.State.Strict (get, put)
import           Data.List                        (find, uncons)
import           Data.Maybe                       (catMaybes, fromJust,
                                                   listToMaybe)
import           Data.Word                        (Word16, Word8)

import qualified Network.Modbus.TCP               as MB

import           CsvParser                        (parseCSVFile,
                                                   serializeCSVFile)
-- import           Modbus                           (fromFloats, getFloats,
--                                                    modSession, word2Float)

import           PrettyPrint                      (ppError, ppMultModData,
                                                   ppMultThreadState,
                                                   ppPlaceholderModData,
                                                   ppRegisters, ppStrWarning,
                                                   ppThreadError, ppUid)
import           Repl.Error                       (handleReplException,
                                                   replRunExceptT,
                                                   runReplClient)

-- import           Repl.Heartbeat                   (heartbeat, listHeartbeat,
--                                                    stopHeartbeat)

import           Control.Concurrent               (forkIO, putMVar, takeMVar,
                                                   tryReadMVar)
import           Control.Exception.Safe           (bracket, catch, catchAny)
import           Data.Data                        (Proxy (..))
import           Data.Range                       (fromBounds)
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
-- cmd input = catchAny (runReplCommand input) handleReplException
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

list :: a -> Repl ()
list _ = liftIO $ do
        putStrLn "List of commands:"
        putStrLn ""
        mapM_ putStrLn commandsCompl
        putStrLn ""
        putStrLn "For help type \":help [command]\""
        putStrLn ""

------------------------------------------------------------------------------------------
-- Undefined
------------------------------------------------------------------------------------------
writeMultipleRegistersWord = undefined
writeMultipleRegistersFloat = undefined
stopHeartbeat = undefined

readModData = undefined

-- readRegistersWord = undefined

writeModData = undefined

-- replImport = undefined
------------------------------------------------------------------------------------------
-- User commands
------------------------------------------------------------------------------------------

replGet :: Repl ReplState
replGet = lift get

replPut :: ReplState -> Repl ()
replPut  = lift . put
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

-- Reads a ModData from the server
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
writeRegisters _ _ _ = invalidCmd

getAddressModValue :: ModValue -> (String, String) -> Either AppError (Word16, ModValue)
getAddressModValue  mv (addr, value) = do
    address <- pReplWord addr
    modvalue <- case mv of
            ModWord _ -> ModWord . Just <$> pReplWord value
            ModFloat _ -> ModFloat . Just <$> pReplFloat value
    return (address, modvalue)

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
            tpu <- replGetTPU
            let address = Address addr
            let protocol = replProtocol state
            heart <- liftIO $ heartBeatSignal protocol timer worker client tpu address
            putHeartBeat heart

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
-- TODO
------------------------------------------------------------------------------------------

replGetTPU :: Repl TPU
replGetTPU = do
    state <- replGet
    let uid = replUId state
    tid <- liftIO $ getNewTID $ replTransactionId state
    return $ setTPU uid tid





-- writeMultipleRegistersWord :: [String] -> Repl ()
-- writeMultipleRegistersWord (address:values) =
--     replWriteRegisters address values (ModWord Nothing)
-- writeMultipleRegistersWord _ = invalidCmd

-- writeMultipleRegistersFloat :: [String] -> Repl ()
-- writeMultipleRegistersFloat (address:values) =
--     replWriteRegisters address values (ModFloat Nothing)
-- writeMultipleRegistersFloat _ = invalidCmd

-- readModData :: [String] -> Repl ()
-- readModData [] = invalidCmd
-- readModData args = do
--     mdata <- replModData <$> lift get
--     let wrapped = except $ mapM (getModByName mdata) args
--     mds <- replRunExceptT wrapped []
--     response <- mapM readMod mds
--     liftIO $ ppMultModData response




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






-- -- Parse the address and values of a list and apply them
-- -- to a read holding registers modbus command
-- replWriteRegisters :: String  -- address
--                    -> [String] -- values
--                    -> ModValue
--                    -> Repl ()
-- replWriteRegisters address values mValue = do
--     connection <- replConn <$> replAsk
--     uid <- replUId <$> lift get
--     tid <- incrementTransactionid
--     let wrapped = do
--             addr <- except $ pReplWord address
--             val <- case mValue of
--                 ModWord _  -> except $ mapM pReplWord values
--                 ModFloat _ -> except $ fromFloats <$> mapM pReplFloat values
--             liftIO $ putStrLn $ "Writing " ++ regsWritten val ++ " register(s) at address " ++ show addr
--             runReplSession connection $ MB.writeMultipleRegisters tid 0 (MB.UnitId uid) (Address addr) val
--     response <- replRunExceptT wrapped 0
--     liftIO $ putStrLn $ responseWritten response ++ " register(s) written"
--   where
--     regsWritten val = show $ divbyModValue $ length val
--     responseWritten resp = show $ divbyModValue resp
--     divbyModValue num = num `div` fromIntegral (toInteger (getModValueMult mValue))



withClient :: ReplState -> (Client -> Repl ()) -> Repl ()
withClient state = bracket getConfig releaseConfig
  where
    getConfig = liftIO $ takeMVar $ replClient state
    releaseConfig client = do
        state' <- replGet
        let clientVar = replClient state'
        liftIO $ putMVar clientVar client
