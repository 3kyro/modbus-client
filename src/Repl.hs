module Repl (
    runRepl,
    ReplState (..),
) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.List (isPrefixOf)
import System.Console.Repline (
    CompleterStyle (Word),
    WordCompleter,
    evalRepl,
 )

import Repl.Commands (cmd, commandsCompl, list)
import Repl.Help (help, helpCompl)
import Types

runRepl :: ReplState -> IO ()
runRepl = evalStateT haskelineStack
  where
    haskelineStack = evalRepl (pure "> ") cmd options (Just ':') (Word completer) ini

ini :: Repl ()
ini = liftIO $ putStrLn "ModBus interactive client\r\nType ':list' for a list of commands, Ctr+D to exit"

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) (commandsCompl ++ helpCompl)

options :: [(String, [String] -> Repl ())]
options =
    [ ("help", help) -- :help
    , ("list", list) -- :help
    ]