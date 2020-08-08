module Repl.Error 
    (
      ReplError (..)
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

import Repl.Types (Repl, ReplError (..))

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