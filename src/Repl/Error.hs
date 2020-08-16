module Repl.Error 
    (
      AppError (..)
    , runReplSession
    , replRunExceptT    
    ) 
    where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except 
    (
      runExceptT
    , ExceptT
    , mapExceptT
    )
import Control.Monad.IO.Class ()
import Data.Either.Combinators (mapLeft)

import qualified System.Modbus.TCP as MB

import AppError (printError)
import Types (Repl, AppError (..))

-- Run a modbus session, converting the left part to AppError
runReplSession :: MB.Connection -> MB.Session a -> ExceptT AppError IO a
runReplSession c s= mapExceptT toReplExcepT $  MB.runSession c s

-- Converts a ModbusException wrapped in IO to a AppError
toReplExcepT :: IO (Either MB.ModbusException a) -> IO (Either AppError a)
toReplExcepT mb = mapLeft AppModbusError <$> mb

-- Rus an ExceptT, returning a default value in case of AppError
replRunExceptT :: ExceptT AppError IO a -> a -> Repl a
replRunExceptT ex rt = do
    unwrapped <- liftIO $ runExceptT ex
    case unwrapped of
        Left err -> liftIO $ printError err >> return rt
        Right x -> return x