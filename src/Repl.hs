module Repl (runRepl, pWord, pFloat) where

import System.Console.Repline (HaskelineT, evalRepl, WordCompleter, CompleterStyle( Word ))
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf, uncons)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Text.Parsec (ParseError, parse, eof, option, string)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (float, decimal, makeTokenParser)
import Text.Parsec.Language (haskellDef)
import Control.Monad.Except (runExceptT)


import qualified System.Modbus.TCP as MB

import Modbus
import CsvParser (ByteOrder)

type Repl a = HaskelineT (ReaderT Config IO) a

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
readInputRegisterWord [] = liftIO $ putStrLn "Invalid command arguments"
readInputRegisterWord [a, n] = do
    let pReturn = getAddressNumber a n
    case pReturn of
        Left err -> liftIO $ print err
        Right (addr, num) -> do
            Config c _ <- ask
            results <- liftIO $ runExceptT $ MB.runSession c $ MB.readInputRegisters 0 0 255 (MB.RegAddress addr) num 
            case results of
                Left mdError -> liftIO $ print mdError
                Right mdData -> do
                    liftIO $ putStrLn $ "Reading " ++ show num ++ " registers from address " ++ show addr
                    liftIO $ mapM_ print mdData
readInputRegisterWord (_:_:_) = liftIO $ putStrLn "Invalid command arguments"

readInputRegisterFloat :: [String] -> Repl ()
readInputRegisterFloat [] = liftIO $ putStrLn "Invalid command arguments"
readInputRegisterFloat [a, n] = do
    let pReturn = getAddressNumber a n
    case pReturn of
        Left err -> liftIO $ print err
        Right (addr, num) -> do
            Config c o <- ask
            results <- liftIO $ runExceptT $ MB.runSession c $ MB.readInputRegisters 0 0 255 (MB.RegAddress addr) (num*2) 
            case results of
                Left mdError -> liftIO $ print mdError
                Right mdData -> do
                    liftIO $ putStrLn $ "Reading " ++ show num ++ " registers from address " ++ show addr
                    liftIO $ mapM_ print (getFloats mdData o)
readInputRegisterFloat (_:_:_) = liftIO $ putStrLn "Invalid command arguments"



getAddressNumber :: String -> String-> Either ParseError (Word16, Word16)
getAddressNumber a n = do
    address <- getWord a
    number <- getWord n
    return (address, number)

getFloats :: [Word16] -> ByteOrder -> [Float]
getFloats [] _ = []
getFloats [_] _ = []
getFloats (x:y:ys) bo = word2Float bo (x,y) : getFloats ys bo 

pFloat :: Parser Float
pFloat = read <$> ((++) <$> sign <*> pf)
  where
      sign = option "" (string "-")
      pf = show <$> float (makeTokenParser haskellDef)   

getFloat :: String -> Either ParseError Float
getFloat = parse (pFloat <* eof) ""
        
pWord :: Parser Word16
pWord = fromInteger <$> decimal (makeTokenParser haskellDef)

getWord :: String -> Either ParseError Word16
getWord = parse (pWord <* eof) ""



