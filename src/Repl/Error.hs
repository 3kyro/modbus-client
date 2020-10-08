module Repl.Error
    ( AppError (..)
    , handleReplException
    , replRunExceptT,runReplClient
    ) where

import           Control.Exception.Safe     (SomeException, try)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Either.Combinators    (mapLeft)


import           PrettyPrint                (ppError)
import           Types                      (AppError (..), Repl)

handleReplException :: SomeException -> IO ()
handleReplException exception = ppError (AppModbusError exception)

-- Rus an ExceptT, returning a default value in case of AppError
replRunExceptT :: ExceptT AppError IO a -> a -> Repl a
replRunExceptT ex rt = do
    unwrapped <- liftIO $ runExceptT ex
    case unwrapped of
        Left err -> liftIO $ ppError err >> return rt
        Right x  -> return x

runReplClient :: IO a -> Repl (Either AppError a)
runReplClient action = liftIO $ mapLeft AppModbusError <$> try action
