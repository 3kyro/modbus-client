module Repl.Commands where

import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.State.Strict (get, put)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Reader (ask)
import Data.List (find, uncons)
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word16)

import qualified System.Modbus.TCP as MB

import CsvParser (serializeCSVFile, parseCSVFile)
import Modbus 
    (
      getFloats
    , fromFloats
    , word2Float
    , modSession
    )
import PrettyPrint (ppMultModData, ppStrWarning, ppRegisters)
import Repl.Error
    (
      runReplSession
    , replRunExceptT
    )
import Repl.Parser
    (
      pReplInt
    , pReplId
    , pReplDesc
    , pReplAddrNum
    , pReplFloat
    , pReplWord
    )
import Types
import System.Modbus.TCP (UnitId(UnitId))

-- Top level command function
cmd :: String -> Repl ()
cmd input = 
  let 
    (command, args) = (fromJust . uncons . words) input
  in case command of
        "readInputRegistersWord" -> readRegistersWord InputRegister args
        "readInputRegistersFloat" -> readRegistersFloat InputRegister args
        "readHoldingRegistersWord" -> readRegistersWord HoldingRegister args
        "readHoldingRegistersFloat" -> readRegistersFloat HoldingRegister args
        "writeSingleRegisterWord" -> writeSingleRegisterWord args
        "writeMultipleRegistersWord" -> writeMultipleRegistersWord args
        "writeSingleRegisterFloat" -> writeSingleRegisterFloat args
        "writeMultipleRegistersFloat" -> writeMultipleRegistersFloat args
        "read" -> readModData args
        "write" -> writeModData args
        "watchdog" -> watchdog args
        "import" -> replImport args
        "export" -> replExport args
        _ -> liftIO $ putStrLn ("command not found: " ++ command)

commandsCompl :: [String]
commandsCompl = [
      "readInputRegistersWord"
    , "readInputRegistersFloat"
    , "readHoldingRegistersWord"
    , "readHoldingRegistersFloat"
    , "writeSingleRegistersWord"
    , "writeMultipleRegistersWord"
    , "writeSingleRegisterFloat"
    , "writeMultipleRegistersFloat"
    , "read"
    , "write"
    , "watchdog"
    , "import"
    , "export"
    ]

list :: a -> Repl ()
list _ = liftIO $ do
        putStrLn ""
        mapM_ putStrLn commandsCompl
        putStrLn ""
        putStrLn "For help type \":help [command]\""
        putStrLn ""

------------------------------------------------------------------------------------------
-- User commands
------------------------------------------------------------------------------------------

-- TODO: Untangle this mess
readRegistersWord :: RegType -> [String] -> Repl ()
readRegistersWord rt [address, number] = do
    let f = case rt of
            InputRegister -> MB.readInputRegisters
            HoldingRegister -> MB.readHoldingRegisters
    response <- replReadRegisters address number (ModWord Nothing) f
    if null response
    then return ()
    else do
        let zipFun = (,) <$> fst <*> (ModWord . Just . snd)
        let zips = zipFun <$> response
        liftIO $ ppRegisters rt zips
readRegistersWord _ _ = invalidCmd

-- TODO: #4 Untangle this mess
readRegistersFloat :: RegType -> [String] -> Repl ()
readRegistersFloat rt [address, number] = do
    Config _ order <- lift $ lift $ ask
    let f = case rt of
            InputRegister -> MB.readInputRegisters
            HoldingRegister -> MB.readHoldingRegisters
    response <- replReadRegisters address number (ModFloat Nothing) f
    if null response
    then return ()
    else do
        let floats = getFloats order (map snd response)
        let zips = zip (incrBy2 (fst (head response))) (toModFloat floats)
        liftIO $ ppRegisters rt zips
  where
    incrBy2 x = iterate (+2) x
    toModFloat xs = (ModFloat . Just) <$> xs
readRegistersFloat _ _ = invalidCmd

writeSingleRegisterWord :: [String] -> Repl ()
writeSingleRegisterWord [address, regValue] =
    replWriteRegisters address [regValue] (ModWord Nothing)
writeSingleRegisterWord _ = invalidCmd

writeMultipleRegistersWord :: [String] -> Repl ()
writeMultipleRegistersWord (address:values) =
    replWriteRegisters address values (ModWord Nothing) 
writeMultipleRegistersWord _ = invalidCmd

writeSingleRegisterFloat :: [String] -> Repl ()
writeSingleRegisterFloat [address, regValue] = 
    replWriteRegisters address [regValue] (ModFloat Nothing) 
writeSingleRegisterFloat _ = invalidCmd

writeMultipleRegistersFloat :: [String] -> Repl ()
writeMultipleRegistersFloat (address:values) = 
    replWriteRegisters address values (ModFloat Nothing) 
writeMultipleRegistersFloat _ = invalidCmd

readModData :: [String] -> Repl ()
readModData [] = invalidCmd
readModData args = do 
    ReplState mdata _ <- lift $ get
    let wrapped = except $ mapM (getModByDescription mdata) args
    mds <- replRunExceptT wrapped []
    response <- mapM readMod mds
    liftIO $ ppMultModData response

writeModData :: [String] -> Repl ()
writeModData [] = invalidCmd
writeModData args = do
    ReplState mdata _ <- lift $ get
    let mPairs = getPairs args
    case mPairs of
        Nothing -> invalidCmd
        Just pairs -> do
            let wrapped = except $ getModByPair pairs mdata
            mds <- replRunExceptT wrapped []
            written <- mapM writeMod mds
            liftIO $ putStrLn $ show (sum written) ++ " value(s) written"
            readback <- mapM readMod mds
            liftIO $ ppMultModData readback

watchdog :: [String] -> Repl ()
watchdog args = do
    let mPairs = getPairs args
    case mPairs of
        Nothing -> invalidCmd
        Just pairs -> do
            addrTimer <- mapM replGetAddrTimer pairs
            mapM_ spawnWatchdogThread addrTimer 

replImport :: [String] -> Repl ()
replImport [] = liftIO $ do 
    putStrLn "Usage: import path-to-csv-file"
    putStrLn "e.g. import ~/path/to/file.csv"
    putStrLn "Type \":help import\" for more information"
replImport [filename] = do
    contents <- liftIO $ parseCSVFile filename
    mdata <- replRunExceptT (except contents) []
    state <- lift $ get
    lift $ put state {replModData = mdata}
    liftIO $ putStrLn $ show (length mdata) ++ " registers updated"
replImport _ = invalidCmd

replExport :: [String] -> Repl ()
replExport [] = liftIO $ do
    putStrLn "Usage: export path-to-csv-file"
    putStrLn "e.g. export ~/path/to/file.csv"
    putStrLn "Type \":help export\" for more information"
replExport [filename] = do
    ReplState mdata _ <- lift $ get
    Config connection order <- replAsk
    let exportSession = modSession mdata order
    currentMdata <- liftIO $ runExceptT $ runReplSession connection exportSession
    mdata' <- replRunExceptT (except currentMdata) []
    liftIO $ serializeCSVFile filename mdata'
    return ()
replExport _ = invalidCmd

-------------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------------
            
-- heartbeat function
heartbeat 
    :: Word16   -- Address of holding register
    -> Int      -- heartbeat period in ms
    -> Word16   -- starting value for accumulator
    ->MB.Connection 
    -> IO ()
heartbeat address timer acc connection= do
    let acc' = acc + 1
    threadDelay timer
    wrapped <- runExceptT $ runReplSession connection $ MB.writeSingleRegister 0 0 255 (MB.RegAddress address) acc'
    case wrapped of
        Left err -> putStrLn $ show err
        Right _ -> heartbeat address timer acc' connection

-- get a watchdog register address and a timer
replGetAddrTimer 
    :: (String, String) -- (ModName or address, timer)
    -> Repl (Word16, Int)
replGetAddrTimer (x,y) = do
    ReplState mdata _ <- lift $ get
    let wrapped = do
            replId <- pReplId x
            timer <- pReplInt y
            case replId of 
                ReplDesc name -> do
                    md <- findModByDesc mdata name
                    return (modAddress md, timer)
                ReplAddr addr -> return (addr, timer)
    replRunExceptT (except wrapped) (0,0)

-- Spawns a watchdog thread
spawnWatchdogThread :: (Word16, Int) -> Repl ()
spawnWatchdogThread (addr, timer)
    | timer <= 0 = return ()
    | otherwise = do
        Config connection _ <- replAsk
        liftIO $ forkIO $ heartbeat addr timer 0 connection
        liftIO $ putStrLn $ "Watchdog launched at address: " ++ show addr ++ ", with an interval of " ++ show timer ++ "ms"  
        
-- Returns a pair of successive values
-- Returns Nothing if the list size is odd
getPairs :: [a] -> Maybe [(a,a)]
getPairs [] = Just []
getPairs [_] = Nothing
getPairs (x:y:zs) = (:) <$> Just (x,y) <*> getPairs zs 

-- Writes a ModData to the server
-- Returns the number of ModData written
writeMod :: ModData -> Repl Word16
writeMod md = do
    let address = modAddress md
    let modVal = modValue md
    let regType = modRegType md
    case regType of
        HoldingRegister -> do
            case modVal of
                ModWord _ -> writeModWord modVal address
                ModFloat _ -> writeModFloat modVal address
        _ -> do
            liftIO $
                ppStrWarning $  "Invalid register type for value: "
                            ++ modName md
            return 0

-- Write a ModWord to the modbus server.
-- Returns the number of ModWords written
writeModWord :: ModValue -> Word16 -> Repl Word16
writeModWord (ModWord (Just word)) address = do
    Config connection _  <- replAsk
    ReplState _ uid <- lift $ get
    let addr = MB.RegAddress address
    let writeSession = runReplSession connection $ MB.writeMultipleRegisters 0 0 (UnitId uid) addr [word]
    replRunExceptT writeSession 0
writeModWord (ModWord _) _ = return 0

-- Write a ModFloat to the modbus server.
-- Returns the number of ModFloats written
writeModFloat :: ModValue -> Word16 -> Repl Word16
writeModFloat (ModFloat (Just fl)) address = do
    Config connection _ <- replAsk
    ReplState _ uid <- lift $ get
    let addr = MB.RegAddress address
    let ordWords = fromFloats [fl]
    let writeSession = runReplSession connection $ MB.writeMultipleRegisters 0 0 (UnitId uid) addr ordWords
    wordsWritten <- replRunExceptT writeSession 0
    return $ wordsWritten `div` 2
    
-- Checks if a list of descriptions are all valid when
-- checked against a list of ModData    
-- All descriptions must exist in the provided list
-- If a description does not exist, a AppError is returned
getModByDescription :: [ModData] -> String -> Either AppError ModData
getModByDescription mdata x = do
    parsed <- pReplDesc x
    findModByDesc mdata parsed
    
findModByDesc :: [ModData] -> String -> Either AppError ModData
findModByDesc mdata x = do 
    let maybeHit = find (\d -> modName d == x) mdata
    case maybeHit of
        Nothing -> Left $ AppCommandError $ "Modbus Register " ++ x ++ " not found on template file." 
        Just hit -> Right hit

-- Generate a list of Moddata from pairs of Description and values
getModByPair :: [(String, String)] -> [ModData] -> Either AppError [ModData]
getModByPair [] _ = Right []
getModByPair ((desc,val):xs) mds = do
    validMd <- getModByDescription mds desc
    inserted <- insertValue validMd val
    (:) <$> return inserted <*> getModByPair xs mds

-- Parses and inserts a value to a ModData
insertValue :: ModData -> String -> Either AppError ModData
insertValue md val = do
    case modValue md of
        ModWord _ -> do
            parsedV <- pReplWord val
            return $ md {modValue = ModWord (Just parsedV)}
        ModFloat _ -> do
            parsedV <- pReplFloat val
            return $ md {modValue = ModFloat (Just parsedV)}

-- Reads a ModData from the server
readMod :: ModData -> Repl ModData
readMod md = do
    Config connection order <- replAsk
    ReplState _ uid <- lift $ get
    let modbusResp = runReplSession connection $ function 0 0 (UnitId uid) address mult
    resp <- replRunExceptT modbusResp []
    case modValue md of
        ModWord _ -> return md {modValue = ModWord $ listToMaybe resp}
        ModFloat _ -> return md {modValue = ModFloat $ floats resp order}
  where
    address = MB.RegAddress $ modAddress md
    mult = getModValueMult $ modValue md
    function = case modRegType md of
            InputRegister -> MB.readInputRegisters
            HoldingRegister -> MB.readHoldingRegisters
            _ -> undefined
    floats response order = word2Float order <$> maybeWords response
    maybeWords response = (,) <$> listToMaybe response <*> listToMaybe (tail response)

-- Parses the address and number of registers strings and applies the given
-- read modbus register function
replReadRegisters :: 
       String   -- address
    -> String   -- number of registers
    -> ModValue
    -> ReadRegsFun
    -> Repl [(Word16, Word16)]
replReadRegisters a n m f = do
    Config connection _ <- replAsk
    ReplState _ uid <- lift $ get
    let mult = getModValueMult m
    let wrapped = do 
            (addr, num) <- except $ pReplAddrNum a n 
            liftIO $ putStrLn $ "Reading " ++ show num ++ " register(s) from address " ++ show addr
            mvs <- runReplSession connection $ f 0 0 (UnitId uid) (MB.RegAddress addr) (mult * num)
            return $ zip [addr..] mvs
    replRunExceptT wrapped []

-- Parses the address and values list and applies them
-- to a read holding registers modbus command
replWriteRegisters :: 
       String  -- address
    -> [String] -- values
    -> ModValue
    -> Repl ()
replWriteRegisters address values mValue = do
    Config connection _ <- replAsk
    ReplState _ uid <- lift $ get
    let wrapped = do
            addr <- except $ pReplWord address
            val <- case mValue of
                ModWord _ -> except $ mapM pReplWord values
                ModFloat _ -> except $ fromFloats <$> mapM pReplFloat values
            liftIO $ putStrLn $ "Writing " ++ regsWritten val ++ " register(s) at address " ++ show addr
            runReplSession connection $ MB.writeMultipleRegisters 0 0 (UnitId uid) (MB.RegAddress addr) val
    response <- replRunExceptT wrapped 0
    liftIO $ putStrLn $ responseWritten response ++ " register(s) written"
  where
    regsWritten val = show $ divbyModValue $ length val
    responseWritten resp = show $ divbyModValue resp   
    divbyModValue num = num `div` fromIntegral (toInteger (getModValueMult mValue)) 

invalidCmd :: Repl ()
invalidCmd = liftIO $ putStrLn "Invalid Command argument"


