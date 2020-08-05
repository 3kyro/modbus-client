module Repl (runRepl) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (runExceptT, ExceptT, except, mapExceptT)
import Control.Monad.IO.Class ()
import Data.List (isPrefixOf, uncons, find)
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word16)
import Data.Either.Combinators (mapLeft)
import System.Console.Repline (HaskelineT, evalRepl, WordCompleter, CompleterStyle( Word ))
import Text.Parsec (ParseError, parse, eof, option, string)
import Text.Parsec.Token (float, decimal, makeTokenParser)
import Text.Parsec.Language (haskellDef)

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
    (
      ModType (..)
    , ModData (..)
    , RegType (..)
    , pName
    )

type Repl a = HaskelineT (ReaderT Config IO) a 

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16]

data ReplError = 
      ReplParseError ParseError
    | ReplModbusError MB.ModbusException
    | ReplCommandError String
    deriving (Show)

runRepl :: Config -> IO ()
runRepl  = runReaderT $ evalRepl (pure "> ") cmd options (Just ':') (Word completer) ini

ini :: Repl ()
ini = liftIO $ putStrLn "ModBus interactive client\r\nType ':help' for a list of commands, Ctr+D to exit"

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
        _ -> liftIO $ putStrLn ("command not found: " ++ command)

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) commands

  -- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> Repl ()
say args = do
  _ <- liftIO $ mapM_ putStrLn args
  return ()

options :: [(String, [String] -> Repl ())] 
options = [
    ("help", help)  -- :help
  , ("say", say)    -- :say
  ]

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
    ]

readRegistersWord :: ReadRegsFun -> [String] -> Repl ()
readRegistersWord f [address, number] = do
    response <- replReadRegisters address number (ModWord Nothing) f
    liftIO $ print response
readRegistersWord _ _ = liftIO $ putStrLn "Invalid command arguments"

readRegistersFloat :: ReadRegsFun -> [String] -> Repl ()
readRegistersFloat f [address, number] = do
    Config _ order _ <- ask
    response <- replReadRegisters address number (ModFloat Nothing) f
    liftIO $ print (getFloats order response)
readRegistersFloat _ _ = liftIO $ putStrLn "Invalid command arguments"

writeSingleRegisterWord :: [String] -> Repl ()
writeSingleRegisterWord [address, regValue] = 
    replWriteRegisters address [regValue] (ModWord Nothing)
writeSingleRegisterWord _ = liftIO $ print "Invalid command arguments"

writeMultipleRegistersWord :: [String] -> Repl ()
writeMultipleRegistersWord (address:values) = 
    replWriteRegisters address values (ModWord Nothing) 
writeMultipleRegistersWord _ = liftIO $ print "Invalid command arguments"

writeSingleRegisterFloat :: [String] -> Repl ()
writeSingleRegisterFloat [address, regValue] = 
    replWriteRegisters address [regValue] (ModFloat Nothing) 
writeSingleRegisterFloat _ = liftIO $ print "Invalid command arguments"

writeMultipleRegistersFloat :: [String] -> Repl ()
writeMultipleRegistersFloat (address:values) = 
    replWriteRegisters address values (ModFloat Nothing) 
writeMultipleRegistersFloat _ = liftIO $ print "Invalid command arguments"

readModData :: [String] -> Repl ()
readModData [] = liftIO $ print "Invalid command arguments"
readModData args = do 
    Config _ _ mdata <- ask
    let wrapped = except $ getModByDescription args mdata
    mds <- replRunExceptT wrapped (const [])
    response <- mapM readMod mds
    liftIO $ mapM_ print response
    
getModByDescription :: [String] -> [ModData] -> Either ReplError [ModData]
getModByDescription [] _ = Right []
getModByDescription (x:xs) mdata = do
    parsed <- mapLeft ReplParseError $ parse pName "" $ T.pack (x ++ ";")
    let maybeHit = find (\d -> modName d == parsed) mdata
    case maybeHit of
        Nothing -> Left $ ReplCommandError $ "Modbus Register " ++ x ++ " not found on template file." 
        Just hit -> (:) <$> return hit <*> getModByDescription xs mdata

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
            (addr, num) <- except $ pAddressNumber a n 
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
            addr <- except $ pWord address
            val <- case modtype of
                ModWord _ -> except $ mapM pWord values
                ModFloat _ -> except $ fromFloats <$> mapM pFloat values
            liftIO $ putStrLn $ "Writing " ++ regsWritten val ++ " register(s) at address " ++ show addr
            runReplSession connection $ MB.writeMultipleRegisters 0 0 255 (MB.RegAddress addr) val
    response <- replRunExceptT wrapped (const 0) 
    liftIO $ putStrLn $ responseWritten response ++ " register(s) written"
  where
    regsWritten val = show $ divbyModtype $ length val
    responseWritten resp = show $ divbyModtype resp   
    divbyModtype num = num `div` fromIntegral (toInteger (getModTypeMult modtype)) 

-- Run a modbus session, converting the left part to ReplError
runReplSession :: MB.Connection -> MB.Session a -> ExceptT ReplError IO a
runReplSession c s= mapExceptT toReplExcepT $  MB.runSession c s

-- Converts a ModbusException wrapped in IO to a ReplError
toReplExcepT :: IO (Either MB.ModbusException a) -> IO (Either ReplError a)
toReplExcepT mb = mapLeft ReplModbusError <$> mb

-- Rus an ExceptT converting a possible Left return
replRunExceptT :: Show b =>  ExceptT b IO a -> (b -> a) -> Repl a
replRunExceptT ex f = do
    unwrapped <- liftIO $ runExceptT ex
    case unwrapped of
        Left err -> liftIO $ print err >> return (f err)
        Right x -> return x 

-- Parse address and number of register strings 
pAddressNumber :: String -> String-> Either ReplError (Word16, Word16)
pAddressNumber a n = (,) <$> pWord a <*> pWord n

pFloat :: String -> Either ReplError Float
pFloat s = mapLeft ReplParseError parseResult
  where 
    parseResult = parse (pRawFloat <* eof) "" s
    pRawFloat = read <$> ((++) <$> sign <*> pf)
    sign = option "" (string "-")
    pf = show <$> float (makeTokenParser haskellDef)   
        
pWord :: String -> Either ReplError Word16
pWord s = mapLeft ReplParseError parseResult  
  where
    parseResult = parse (pRawWord <* eof) "" s
    pRawWord = fromInteger <$> decimal (makeTokenParser haskellDef)

getModTypeMult :: ModType -> Word16
getModTypeMult (ModWord _) = 1
getModTypeMult (ModFloat _) = 2


