module Repl (runRepl) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Except (runExceptT, ExceptT, except, mapExceptT)
import Control.Monad.IO.Class ()
import Data.List (isPrefixOf, uncons)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import Data.Either.Combinators (mapLeft)
import System.Console.Repline (HaskelineT, evalRepl, WordCompleter, CompleterStyle( Word ))
import Text.Parsec (ParseError, parse, eof, option, string)
import Text.Parsec.Token (float, decimal, makeTokenParser)
import Text.Parsec.Language (haskellDef)

import qualified System.Modbus.TCP as MB

import Modbus 
    (
      Config (..)
    , getFloats
    , fromFloats
    )
import CsvParser (ModType (..))

type Repl a = HaskelineT (ReaderT Config IO) a

type ReadRegsFun =  MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16]

data ReplError = 
    ReplParseError ParseError
    | ReplModbusError MB.ModbusException
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
    ]

readRegistersWord :: ReadRegsFun -> [String] -> Repl ()
readRegistersWord f [address, number] = do
    Config connection _ <- ask
    response <- replReadRegisters address number (ModWord Nothing) connection f
    liftIO $ print response
readRegistersWord _ _ = liftIO $ putStrLn "Invalid command arguments"

readRegistersFloat :: ReadRegsFun -> [String] -> Repl ()
readRegistersFloat f [address, number] = do
    Config connection order <- ask
    response <- replReadRegisters address number (ModFloat Nothing) connection f
    liftIO $ print (getFloats response order)
readRegistersFloat _ _ = liftIO $ putStrLn "Invalid command arguments"

writeSingleRegisterWord :: [String] -> Repl ()
writeSingleRegisterWord [address, value] = do
    Config connection _ <- ask
    replWriteRegisters address [value] (ModWord Nothing) connection
writeSingleRegisterWord _ = liftIO $ print "Invalid command arguments"

writeMultipleRegistersWord :: [String] -> Repl ()
writeMultipleRegistersWord (address:values) = do
    Config connection _ <- ask
    replWriteRegisters address values (ModWord Nothing) connection
writeMultipleRegistersWord _ = liftIO $ print "Invalid command arguments"

writeSingleRegisterFloat :: [String] -> Repl ()
writeSingleRegisterFloat [address, value] = do
    Config connection _ <- ask
    replWriteRegisters address [value] (ModFloat Nothing) connection
writeSingleRegisterFloat _ = liftIO $ print "Invalid command arguments"

writeMultipleRegistersFloat :: [String] -> Repl ()
writeMultipleRegistersFloat (address:values) = do
    Config connection _ <- ask
    replWriteRegisters address values (ModFloat Nothing) connection
writeMultipleRegistersFloat _ = liftIO $ print "Invalid command arguments"

-- Parses the address and number of registers strings and applies the given
-- read modbus register function
replReadRegisters :: 
       String   -- address
    -> String   -- number of registers
    -> ModType
    -> MB.Connection 
    -> ReadRegsFun
    -> Repl [Word16]
replReadRegisters a n m connection f = do
    let mult = getModTypeMult m
    let wrapped = do 
            (addr, num) <- pAddressNumber a n 
            liftIO $ putStrLn $ "Reading " ++ show num ++ " register(s) from address " ++ show addr
            runReplSession connection $ f 0 0 255 (MB.RegAddress addr) (mult * num)
    unwrapped <- liftIO $ runExceptT wrapped
    case unwrapped of
        Left mdError -> liftIO $ print mdError >> return []
        Right mdData -> return mdData

-- Parses the address and values list and applies them
-- to a read holding registers modbus command
replWriteRegisters :: 
       String  -- address
    -> [String] -- values
    -> ModType
    -> MB.Connection
    -> Repl ()
replWriteRegisters address values modtype connection = do
    let wrapped = do
            addr <- except $ pWord address
            val <- case modtype of
                ModWord _ -> except $ mapM pWord values
                ModFloat _ -> except $ fromFloats <$> mapM pFloat values
            liftIO $ putStrLn $ "Writing " ++ regsWritten val ++ " register(s) at address " ++ show addr
            runReplSession connection $ MB.writeMultipleRegisters 0 0 255 (MB.RegAddress addr) val
    unwrapped <- liftIO $ runExceptT wrapped    
    case unwrapped of
        Left mdError -> liftIO $ print mdError
        Right response -> liftIO $ putStrLn $ responseWritten response ++ " register(s) written"
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

-- Parse address and number of register strings 
pAddressNumber :: String -> String-> ExceptT ReplError IO (Word16, Word16)
pAddressNumber a n = do
    address <- except $ pWord a
    number <- except $ pWord n
    return (address, number)

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
