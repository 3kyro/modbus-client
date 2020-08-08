module Repl.Help (help) where

import Control.Monad.Trans (liftIO)

import Repl.Types (Repl)

-- Top level help command
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args
