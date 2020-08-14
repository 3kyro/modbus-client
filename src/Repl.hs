module Repl (runRepl, ReplConfig(..), ReplState(..)) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.List (isPrefixOf)
import System.Console.Repline 
    (
       evalRepl
     , WordCompleter
     , CompleterStyle( Word )
    )

import Types 
import Repl.Commands (cmd, commandsCompl, list)
import Repl.Help (help, helpCompl)

runRepl :: ReplConfig -> ReplState -> IO ()
runRepl conf state = runReaderT (stateStack state) conf
  where 
    stateStack = evalStateT $ haskelineStack
    haskelineStack = evalRepl (pure "> ") cmd options (Just ':') (Word completer) ini

ini :: Repl ()
ini = liftIO $ putStrLn "ModBus interactive client\r\nType ':list' for a list of commands, Ctr+D to exit"

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) (commandsCompl ++ helpCompl)

options :: [(String, [String] -> Repl ())] 
options = [
      ("help", help)  -- :help
    , ("list", list)  -- :help
  ]


