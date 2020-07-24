{-# LANGUAGE FlexibleInstances #-}
module Repl (runRepl) where

import System.Console.Repline (HaskelineT, evalRepl, WordCompleter, CompleterStyle( Word ))
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf, uncons)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Text.Parsec (ParseError, parse, eof, option, string)
import Text.Parsec.Token (float, decimal, makeTokenParser)
import Text.Parsec.Language (haskellDef)
import Control.Monad.Trans.Except (runExceptT, ExceptT, except, mapExceptT)
import Data.Either.Combinators (mapLeft)
import Control.Monad.IO.Class ()

import qualified System.Modbus.TCP as MB

import Modbus (Config (..), word2Float)
import CsvParser (ByteOrder)

type Repl a = HaskelineT (ReaderT Config IO) a

data ReplError = 
    ReplParseError ParseError
    | ReplModbusError MB.ModbusException
    deriving (Show)

runRepl :: Config -> IO ()
runRepl  = runReaderT $ evalRepl (pure ">") cmd options (Just ':') (Word completer) ini

ini :: Repl ()
ini = liftIO $ putStrLn "ModBus interactive client\r\nType ':help' for a list of commands, Ctr+D to exit"

cmd :: String -> Repl ()
cmd input = 
  let 
    (command, args) = (fromJust . uncons . words) input
  in case command of
        "readInputRegisterWord" -> readInputRegisterWord args
        "readInputRegisterFloat" -> readInputRegisterFloat args
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
      "readInputRegisterWord"
    , "readInputRegisterFloat"
    ]

readInputRegisterWord :: [String] -> Repl ()
readInputRegisterWord [address, num] = do
    Config connection _ <- ask
    let wrapped = replReadRegisters address num connection MB.readInputRegisters
    unwrapped <- liftIO $ runExceptT wrapped    
    case unwrapped of
        Left mdError -> liftIO $ print mdError
        Right mdData -> liftIO $ mapM_ print mdData
readInputRegisterWord _ = liftIO $ putStrLn "Invalid command arguments"

readInputRegisterFloat :: [String] -> Repl ()
readInputRegisterFloat [address, number] = do
    Config connection order <- ask
    let wrapped = replReadRegisters address number connection MB.readInputRegisters
    unwrapped <- liftIO $ runExceptT wrapped    
    case unwrapped of
        Left mdError -> liftIO $ print mdError
        Right mdData -> liftIO $ mapM_ print (getFloats mdData order)
readInputRegisterFloat _ = liftIO $ putStrLn "Invalid command arguments"

-- Run a modbus session, converting the left part to ReplError
runReplSession :: MB.Connection -> MB.Session a -> ExceptT ReplError IO a
runReplSession c s= mapExceptT toReplExcepT $  MB.runSession c s

-- Converts a ModbusException wrapped in IO to a ReplError
toReplExcepT :: IO (Either MB.ModbusException a) -> IO (Either ReplError a)
toReplExcepT mb = mapLeft ReplModbusError <$> mb

-- Parses the applied address and number of registers strings and applies the given
-- read modbus register function
replReadRegisters :: String 
               -> String 
               -> MB.Connection 
               -> (MB.TransactionId -> MB.ProtocolId -> MB.UnitId -> MB.RegAddress -> Word16 -> MB.Session [Word16])
               -> ExceptT ReplError IO [Word16]
replReadRegisters a n connection f = do
    (addr, num) <- pAddressNumber a n
    liftIO $ putStrLn $ "Reading " ++ show num ++ " registers from address " ++ show addr
    runReplSession connection $ f 0 0 255 (MB.RegAddress addr) num

-- Parse address and number of register strings 
pAddressNumber :: String -> String-> ExceptT ReplError IO (Word16, Word16)
pAddressNumber a n = do
    address <- except $ pWord a
    number <- except $ pWord n
    return (address, number)

getFloats :: [Word16] -> ByteOrder -> [Float]
getFloats [] _ = []
getFloats [_] _ = []
getFloats (x:y:ys) bo = word2Float bo (x,y) : getFloats ys bo 

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


