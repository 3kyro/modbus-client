{-# LANGUAGE RankNTypes #-}
module Repl.Error
    (
      AppError (..)
    ,  handleReplException
    ,replRunExceptT)
    where

import           Control.Monad.IO.Class     ()
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT)
import           Data.Either.Combinators    (mapLeft)


import qualified Network.Modbus.TCP         as MB


import           Control.Concurrent     (MVar)
import           Control.Exception.Safe (SomeException, catch, catchAny)
import           Data.Tagged            (Tagged)

import           PrettyPrint                (ppError)
import           Types                      (AppError (..), Repl)
import           Types.Modbus (Session, Worker, runClient)










handleReplException :: SomeException -> IO ()
-- handleReplException exception = liftIO $ ppError (AppModbusError exception)
handleReplException exception = ppError (AppModbusError exception)



-- Run a modbus session, converting the left part to AppError
-- runReplSession :: MB.Connection -> MB.Session a -> ExceptT AppError IO a
-- runReplSession c s= mapExceptT toReplExcepT $  MB.runSession c s
-- runReplSession :: Worker IO -> MVar a -> Tagged a (Session IO b) -> IO ()
-- runReplSession worker client session =
--     catchAny rlt someExceptionHandler
--   where rlt = runClient worker client session



-- Converts a ModbusException wrapped in IO to a AppError
-- toReplExcepT :: IO (Either MB.ModbusException a) -> IO (Either AppError a)
-- toReplExcepT mb = mapLeft AppModbusError <$> mb



-- Rus an ExceptT, returning a default value in case of AppError
replRunExceptT :: ExceptT AppError IO a -> a -> Repl a
replRunExceptT ex rt = do
    unwrapped <- liftIO $ runExceptT ex
    case unwrapped of
        Left err -> liftIO $ ppError err >> return rt
        Right x  -> return x


