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
import           Data.Maybe                       (fromJust, listToMaybe)
import           Data.Word                        (Word16)

import qualified Network.Modbus.TCP               as MB

import           CsvParser                        (parseCSVFile,
                                                   serializeCSVFile)
-- import           Modbus                           (fromFloats, getFloats,
--                                                    modSession, word2Float)

import           PrettyPrint                      (ppMultThreadState, ppThreadError, ppError, ppMultModData,
                                                   ppRegisters, ppStrWarning,
                                                   ppUid)
import           Repl.Error                       (replRunExceptT, handleReplException)

-- import           Repl.Heartbeat                   (heartbeat, listHeartbeat,
--                                                    stopHeartbeat)

import           Repl.HelpFun                     (findModByName, getModByName, getModByPair,
                                                   getPairs, invalidCmd)
import           Repl.Parser                      (pReplInt, pReplArg, pReplAddrNum, pReplFloat,
                                                   pReplWord)
import           Types
import Control.Exception.Safe (bracket, catchAny)
import Control.Concurrent (forkIO, putMVar, takeMVar, tryReadMVar)

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
cmd input = runReplCommand input

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
readRegistersWord = undefined
readRegistersFloat = undefined
writeMultipleRegistersWord = undefined
writeMultipleRegistersFloat = undefined
stopHeartbeat = undefined

readModData = undefined

replExport = undefined

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
-- HeartBeat
------------------------------------------------------------------------------------------

withClient :: ReplState -> (Client -> Repl ()) -> Repl ()
withClient state = bracket getConfig releaseConfig
  where
    getConfig = liftIO $ takeMVar $ replClient state
    releaseConfig client = do
        state' <- replGet
        let clientVar = replClient state'
        liftIO $ putMVar clientVar client


replImport :: [String] -> Repl ()
replImport _ = do
    client <- replClient <$> replGet
    liftIO $ forkIO $ keepAliveThread client 2000
    return ()
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
            state <- replGet
            let worker = replDirectWorker state
            let client = replClient state
            let uid = replUId state
            let tpu = setTPU uid 0
            let address = Address addr
            let protocol = replProtocol state
            heart <- liftIO $ heartBeatSignal protocol timer worker client tpu address
            putHeartBeat heart
        -- afterActiveThreadCheck addr $ do
        --     uid <- replUId <$> lift get
        --     newV <- liftIO newEmptyMVar
        --     sock <- replSockAddr <$> replAsk
        --     tm <- replTimeout <$> replAsk
        --     thread <- liftIO $ forkFinally (spawn addr uid timer sock tm newV) (setThreadMVar newV)
        --     let threadS = ThreadState addr timer thread newV
        --     putThreadState threadS
        --     liftIO $ putStrLn $ "Watchdog launched at address: " ++ show addr ++ ", with an interval of " ++ show timer ++ "ms"

-- Put the provided thread in the pool
putHeartBeat :: HeartBeat -> Repl ()
putHeartBeat st = do
    state <- replGet
    let pool = replPool state
    replPut $ state { replPool = st:pool }

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
                    "A watchdog is already running at address " ++ show addr
        Nothing -> action

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
checkThreads :: [HeartBeat] -> IO [HeartBeat]
checkThreads [] = return []
checkThreads (x:xs) = do
    checked <- tryReadMVar (hbStatus x)
    case checked of
        Nothing -> (:) <$> return x <*> checkThreads xs
        Just err -> do
            -- ppThreadError x err
            print err
            checkThreads xs

-- List all active heartbeat signals
listHeartbeat :: [String] -> Repl ()
listHeartbeat [] = do
    checkActiveHeartbeat
    pool <- replPool <$> lift get
    if null pool

    then liftIO $ ppStrWarning "No active watchdog timers"
    else liftIO $ ppMultThreadState pool
listHeartbeat _ = undefined

------------------------------------------------------------------------------------------
-- TODO
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

-- replImport :: [String] -> Repl ()
-- replImport [] = liftIO $ do
--     putStrLn "Usage: import path-to-csv-file"
--     putStrLn "e.g. import ~/path/to/file.csv"
--     putStrLn "Type \":help import\" for more information"
-- replImport [filename] = do
--     contents <- liftIO $ parseCSVFile filename
--     mdata <- replRunExceptT (except contents) []
--     state <- lift get
--     lift $ put state {replModData = mdata}
--     liftIO $ putStrLn $ show (length mdata) ++ " registers updated"
-- replImport _ = invalidCmd

-- replExport :: [String] -> Repl ()
-- replExport [] = liftIO $ do
--     putStrLn "Usage: export path-to-csv-file"
--     putStrLn "e.g. export ~/path/to/file.csv"
--     putStrLn "Type \":help export\" for more information"
-- replExport [filename] = do
--     mdata <- replModData <$> lift get
--     let exportSession = modSession mdata order
--     currentMdata <- liftIO $ runExceptT $ runReplSession connection exportSession
--     mdata' <- replRunExceptT (except currentMdata) []
--     _ <- liftIO $ serializeCSVFile filename mdata'
--     return ()
-- replExport _ = invalidCmd

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

-- -- Reads a ModData from the server
-- readMod :: ModData -> Repl a ModData
-- readMod md = do
--     state <- lift get
--     let uid = replUId state
--     tid <- incrementTransactionid
--     let modbusResp = runReplSession connection $ function tid 0 (MB.UnitId uid) address mult
--     resp <- replRunExceptT modbusResp []
--     case modValue md of
--         ModWord _  -> return md {modValue = ModWord $ listToMaybe resp}
--         ModFloat _ -> return md {modValue = ModFloat $ floats resp order}
--   where
--     address = Address $ modAddress md
--     mult = getModValueMult $ modValue md
--     function = getReadFunction $ modRegType md
--     floats response order = word2Float order <$> maybeWords response
--     maybeWords response = (,) <$> listToMaybe response <*> listToMaybe (tail response)

-- -- get the proper modbus read function
-- getReadFunction :: RegType -> ReadRegsFun
-- getReadFunction rt = case rt of
--     InputRegister   -> MB.readInputRegisters
--     HoldingRegister -> MB.readHoldingRegisters
--     _               -> undefined

-- -- Parses the address and number of registers strings and applies the given
-- -- read modbus register function
-- replReadRegisters :: String   -- address
--                   -> String   -- number of registers
--                   -> ModValue
--                   -> ReadRegsFun
--                   -> Repl a [(Word16, Word16)]
-- replReadRegisters a n m f = do
--     connection <- replConn <$> replAsk
--     uid <- replUId <$> lift get
--     tid <- incrementTransactionid
--     let mult = getModValueMult m
--     let wrapped = do
--             (addr, num) <- except $ pReplAddrNum a n
--             liftIO $ putStrLn $ "Reading " ++ show num ++ " register(s) from address " ++ show addr
--             mvs <- runReplSession connection $ f tid 0 (MB.UnitId uid) (Address addr) (mult * num)
--             return $ zip [addr..] mvs
--     replRunExceptT wrapped []

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

-- -- Increment the transcation id counter and return value to be used
-- -- when contructing a MB.Session
-- incrementTransactionid :: Repl a MB.TransactionId
-- incrementTransactionid = do
--     state@(ReplState _ _ _ _ tid _) <- lift get
--     let tid' = tid + 1
--     lift $ put state {replTransactionId = tid'}
--     return $ MB.TransactionId tid'

