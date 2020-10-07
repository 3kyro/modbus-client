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
import           Data.Maybe                       (catMaybes, fromJust, listToMaybe)
import           Data.Word                        (Word16)

import qualified Network.Modbus.TCP               as MB

import           CsvParser                        (parseCSVFile,
                                                   serializeCSVFile)
-- import           Modbus                           (fromFloats, getFloats,
--                                                    modSession, word2Float)

import           PrettyPrint                      (ppError, ppMultModData,
                                                   ppMultThreadState,
                                                   ppRegisters, ppStrWarning,
                                                   ppThreadError, ppUid)
import           Repl.Error                       (runReplClient, handleReplException,
                                                   replRunExceptT)

-- import           Repl.Heartbeat                   (heartbeat, listHeartbeat,
--                                                    stopHeartbeat)

import           Control.Concurrent               (forkIO, putMVar, takeMVar,
                                                   tryReadMVar)
import           Control.Exception.Safe           (catch, bracket, catchAny)
import           Repl.HelpFun                     (findModByName, getModByName,
                                                   getModByPair, getPairs,
                                                   invalidCmd)
import           Repl.Parser                      (pReplAddrNum, pReplArg,
                                                   pReplFloat, pReplInt,
                                                   pReplWord)
import           Types
import Data.Range (fromBounds)
import Data.Data (Proxy(..), Proxy)

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
        ReadInputRegistersWord -> readRegistersWord InputRegister args
        ReadInputRegistersFloat -> readRegistersFloat InputRegister args
        ReadHoldingRegistersWord -> readRegistersWord HoldingRegister args
        ReadHoldingRegistersFloat -> readRegistersFloat HoldingRegister args
        WriteRegistersWord -> writeMultipleRegistersWord args
        WriteRegistersFloat -> writeMultipleRegistersFloat args
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


readRegistersWord = undefined
readRegistersFloat = undefined

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
-- Registers
------------------------------------------------------------------------------------------

-- readRegistersWord :: RegType -> [String] -> Repl ()
-- readRegistersWord rt [address, number] = do
--     let f = getReadFunction rt
--     response <- replReadRegisters address number (ModWord Nothing) f
--     if null response
--     then return ()
--     else do
--         let zipFun = (,) <$> fst <*> (ModWord . Just . snd)
--         let zips = zipFun <$> response
--         liftIO $ ppRegisters rt zips
-- readRegistersWord _ _ = invalidCmd

-- readRegistersFloat :: RegType -> [String] -> Repl ()
-- readRegistersFloat rt [address, number] = do
--     order <- replOrd <$> replAsk
--     let f = getReadFunction rt
--     response <- replReadRegisters address number (ModFloat Nothing) f
--     if null response
--     then return ()
--     else do
--         let floats = getFloats order (map snd response)
--         let zips = zip (incrBy2 (fst (head response))) (toModFloat floats)
--         liftIO $ ppRegisters rt zips
--   where
--     incrBy2 = iterate (+2)
--     toModFloat xs = ModFloat . Just <$> xs
-- readRegistersFloat _ _ = invalidCmd

-- -- Parses the address and number of registers strings and
-- -- invokes the correct modbus read function
-- replReadRegisters :: String     -- Address
--                   -> String     -- Number of registers
--                   -> RegType    -- Register Type
--                   -> Int        -- Word16 per value multiplier
--                   -> Repl [(Word16, Word16)]
-- replReadRegisters addrStr numStr regType mult = do
--     let wrapped = do
--         (addr, num) <- pReplAddrNum addrStr numStr
--         let range = fromBounds addr (num * mult)
--         let session = case regType of
--                 InputRegister -> readInputRegisters protocol tpu range
--                 HoldingRegister -> readHoldingRegisters protocol tpu range
--         runReplClient $ runClient worker client session

--     liftIO $ putStrLn $ "Reading " ++ show num ++ " register(s) from address " ++ show addr
--     mvs <- runReplSession connection $ f tid 0 (MB.UnitId uid) (Address addr) (mult * num)
--     return $ zip [addr..] mvs



-- -- get the proper modbus read function
-- getReadFunction :: RegType -> ReadRegsFun
-- getReadFunction rt = case rt of
--     InputRegister   -> MB.readInputRegisters
--     HoldingRegister -> MB.readHoldingRegisters
--     _               -> undefined
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
            newTid <- liftIO $ getNewTID tid
            let tpu = setTPU uid newTid
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
    mdata' <- replReeadModData mdata
    liftIO $ serializeCSVFile filename mdata'
    return ()
replExport _ = invalidCmd

-- Reads a ModData from the server
replReeadModData :: [ModData] -> Repl [ModData]
replReeadModData mds = do
    state <- replGet
    let protocol = replProtocol state
    tpu <- replGetTPU
    let order = replByteOrder state
    let proxy = Proxy :: Proxy Client
    let exportSessions = map (readMBRegister proxy protocol tpu order) mds
    let worker = replBatchWorker state
    let client = replClient state
    maybemdata <- runReplClient $ mapM (runClient worker client) exportSessions
    mdata' <- replRunExceptT (except maybemdata) []
    return $ catMaybes mdata'

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

-- writeModData :: [String] -> Repl ()
-- writeModData [] = invalidCmd
-- writeModData args = do
--     mdata <- replModData <$> lift get
--     let mPairs = getPairs args
--     case mPairs of
--         Nothing -> invalidCmd
--         Just pairs -> do
--             let wrapped = except $ getModByPair pairs mdata
--             mds <- replRunExceptT wrapped []
--             written <- mapM writeMod mds
--             liftIO $ putStrLn $ show (sum written) ++ " value(s) written"
--             readback <- mapM readMod mds
--             liftIO $ ppMultModData readback


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

-- -- Write a ModData to the server
-- -- Return the number of ModData written
-- writeMod :: ModData -> Repl Word16
-- writeMod md = do
--     let address = modAddress md
--     let modVal = modValue md
--     let regType = modRegType md
--     case regType of
--         HoldingRegister -> writeModType modVal address
--         _ -> do
--             liftIO $ ppStrWarning $
--                     "Invalid register type for value: "
--                     ++ modName md
--             return 0
  -- Write a ModValue to the modbus server.
-- -- Return the number of items written
-- writeModType :: ModValue -> Word16 -> Repl Word16
-- writeModType value address = do
--     connection <- replConn <$> replAsk
--     uid <- replUId <$> lift get
--     let addr = Address address
--     case value of
--         ModWord (Just word) -> do
--             tid <- incrementTransactionid
--             let writeSession = runReplSession connection $ MB.writeMultipleRegisters tid 0 (MB.UnitId uid) addr [word]
--             replRunExceptT writeSession 0
--         ModFloat (Just float) -> do
--             tid <- incrementTransactionid
--             let ordWords = fromFloats [floa --             let writeSession = runReplSession connection $ MB.writeMultipleRegisters tid 0 (MB.UnitId uid) addr ordWords
--             wordsWritten <- replRunExceptT writeSession 0
--             return $ wordsWritten `div` 2
--         _ -> return 0







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
