module Repl (runRepl, ReplConfig(..), ReplState(..)) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.List (isPrefixOf)
import System.Console.Repline 
    (
       evalRepl
     , WordCompleter
     , CompleterStyle( Word )
    )

import Repl.Types (ReplState(..), Repl, ReplConfig (..))
import Repl.Commands (cmd, commandsCompl, list)
import Repl.Help (help, helpCompl)
import Control.Monad.Trans.State.Strict (evalStateT)

runRepl :: ReplConfig -> ReplState -> IO ()
runRepl conf state = runReaderT (stateStack state) conf
  where 
    stateStack = evalStateT $ haskelineStack
    haskelineStack = evalRepl (pure "> ") cmd options (Just ':') (Word completer) ini

ini :: Repl ()
ini = liftIO $ putStrLn "ModBus interactive client\r\nType ':help' for a list of commands, Ctr+D to exit"

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) (commandsCompl ++ helpCompl)

options :: [(String, [String] -> Repl ())] 
options = [
      ("help", help)  -- :help
    , ("list", list)  -- :help
  ]


