module Repl.Commands where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (zipWithM, ask)
import Control.Monad.Trans.Except (except)
import Control.Monad.IO.Class ()
import Data.List (find, uncons)
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word16)
import Data.Either.Combinators (mapLeft)
import Text.Parsec (parse)

import qualified System.Modbus.TCP as MB
import qualified Data.Text as T

import Modbus 
    (
      Config (..)
    , getFloats
    , fromFloats
    , word2Float
    )
    
import CsvParser 
    (getModTypeMult, 
      ModType (..)
    , ModData (..)
    , RegType (..)
    , pName
    
    )

import Repl.Types (Repl, ReadRegsFun)
import Repl.Error (runReplSession, ReplError (..), replRunExceptT)
import Repl.Parser (pReplAddrNum, pReplFloat, pReplWord)

-- Top level command function
cmd :: String -> Repl ()
cmd input = 
  let 
    (command, args) = (fromJust . uncons . words) input
  in case command of
        "readInputRegistersWord" -> readRegistersWord MB.readInputRegisters args
        "readInputRegistersFloat" -> readRegistersFloat MB.readInputRegisters args
        "readHoldingRegistersWord" -> readRegistersWord MB.readHoldingRegisters args
        "readHoldingRegistersFloat" -> readRegistersFloat MB.readHoldingRegisters args
        "writeSingleRegisterWord" -> writeSingleRegisterWord args
        "writeMultipleRegistersWord" -> writeMultipleRegistersWord args
        "writeSingleRegisterFloat" -> writeSingleRegisterFloat args
        "writeMultipleRegistersFloat" -> writeMultipleRegistersFloat args
        "read" -> readModData args
        "write" -> writeModData args
        _ -> liftIO $ putStrLn ("command not found: " ++ command)

commands :: [String]
commands = [
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
    ]
    
------------------------------------------------------------------------------------------
-- User commands
------------------------------------------------------------------------------------------

readRegistersWord :: ReadRegsFun -> [String] -> Repl ()
readRegistersWord f [address, number] = do
    response <- replReadRegisters address number (ModWord Nothing) f
    liftIO $ print response
readRegistersWord _ _ = invalidCmd

readRegistersFloat :: ReadRegsFun -> [String] -> Repl ()
readRegistersFloat f [address, number] = do
    Config _ order _ <- ask
    response <- replReadRegisters address number (ModFloat Nothing) f
    liftIO $ print (getFloats order response)
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
    Config _ _ mdata <- ask
    let wrapped = except $ getModByDescription args mdata
    mds <- replRunExceptT wrapped (const [])
    response <- mapM readMod mds
    liftIO $ mapM_ print response

writeModData :: [String] -> Repl ()
writeModData [] = invalidCmd
writeModData args = do
    Config _ _ mdata <- ask
    let mPairs = getPairs args
    case mPairs of
        Nothing -> invalidCmd
        Just pairs -> do
            let wrapped = except $ getModByPair pairs mdata
            mds <- replRunExceptT wrapped  (const [])
            written <- mapM writeMod mds  
            liftIO $ putStrLn $ show (sum written) ++ " value(s) written" 

-------------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------------
            
-- Returns a pair of successive values
-- Returns Nothing if the list size is odd
getPairs :: [a] -> Maybe ([a], [a])
getPairs [] = Just ([],[])
getPairs [_] = Nothing
getPairs (x:y:zs) = (,) <$> firsts <*> seconds
  where 
    rest = getPairs zs
    firsts =  (:) <$> Just x <*> (fst <$> rest)
    seconds = (:) <$> Just y <*> (snd <$> rest)

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
                putStrLn $  "Invalid register type for value: " 
                            ++ modName md
            return 0 

-- Write a ModWord to the modbus server.
-- Returns the number of ModWords written
writeModWord :: ModType -> Word16 -> Repl Word16
writeModWord (ModWord (Just word)) address = do
    Config connection _ _ <- ask
    let addr = MB.RegAddress address
    let writeSession = runReplSession connection $ MB.writeMultipleRegisters 0 0 255 addr [word]
    replRunExceptT writeSession (const 0)  
writeModWord (ModWord _) _ = return 0

-- Write a ModFloat to the modbus server.
-- Returns the number of ModFloats written
writeModFloat :: ModType -> Word16 -> Repl Word16
writeModFloat (ModFloat (Just fl)) address = do
    Config connection _ _ <- ask
    let addr = MB.RegAddress address
    let ordWords = fromFloats [fl]
    let writeSession = runReplSession connection $ MB.writeMultipleRegisters 0 0 255 addr ordWords
    wordsWritten <- replRunExceptT writeSession (const 0)
    return $ wordsWritten `div` 2
    
-- Checks if a list of descriptions are all valid when
-- checked against a list of ModData    
-- All descriptions must exist in the provided list
-- If a description does not exist, a ReplError is returned
getModByDescription :: [String] -> [ModData] -> Either ReplError [ModData]
getModByDescription [] _ = Right []
getModByDescription (x:xs) mdata = do
    parsed <- mapLeft ReplParseError $ parse pName "" $ T.pack (x ++ ";")
    let maybeHit = find (\d -> modName d == parsed) mdata
    case maybeHit of
        Nothing -> Left $ ReplCommandError $ "Modbus Register " ++ x ++ " not found on template file." 
        Just hit -> (:) <$> return hit <*> getModByDescription xs mdata

-- Generate a list of Moddata from pairs of Description and values
getModByPair :: ([String], [String]) -> [ModData] -> Either ReplError [ModData]
getModByPair ([], _) _ = Right []
getModByPair (descs, vals) mds = do
    validMd <- getModByDescription descs mds
    zipWithM insertValue validMd vals

-- Parses and inserts a value to a ModData
insertValue :: ModData -> String -> Either ReplError ModData
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
    Config connection order _ <- ask
    let modbusResp = runReplSession connection $ function 0 0 255 address mult
    resp <- replRunExceptT modbusResp (const [])
    case modValue md of
        ModWord _ -> return md {modValue = ModWord $ listToMaybe resp}
        ModFloat _ -> return md {modValue = ModFloat $ floats resp order}
  where
    address = MB.RegAddress $ modAddress md
    mult = getModTypeMult $ modValue md
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
    -> ModType
    -> ReadRegsFun
    -> Repl [Word16]
replReadRegisters a n m f = do
    Config connection _ _ <- ask
    let mult = getModTypeMult m
    let wrapped = do 
            (addr, num) <- except $ pReplAddrNum a n 
            liftIO $ putStrLn $ "Reading " ++ show num ++ " register(s) from address " ++ show addr
            runReplSession connection $ f 0 0 255 (MB.RegAddress addr) (mult * num)
    replRunExceptT wrapped (const [])

-- Parses the address and values list and applies them
-- to a read holding registers modbus command
replWriteRegisters :: 
       String  -- address
    -> [String] -- values
    -> ModType
    -> Repl ()
replWriteRegisters address values modtype = do
    Config connection _ _ <- ask
    let wrapped = do
            addr <- except $ pReplWord address
            val <- case modtype of
                ModWord _ -> except $ mapM pReplWord values
                ModFloat _ -> except $ fromFloats <$> mapM pReplFloat values
            liftIO $ putStrLn $ "Writing " ++ regsWritten val ++ " register(s) at address " ++ show addr
            runReplSession connection $ MB.writeMultipleRegisters 0 0 255 (MB.RegAddress addr) val
    response <- replRunExceptT wrapped (const 0) 
    liftIO $ putStrLn $ responseWritten response ++ " register(s) written"
  where
    regsWritten val = show $ divbyModtype $ length val
    responseWritten resp = show $ divbyModtype resp   
    divbyModtype num = num `div` fromIntegral (toInteger (getModTypeMult modtype)) 


invalidCmd :: Repl ()
invalidCmd = liftIO $ putStrLn "Invalid Command argument"


