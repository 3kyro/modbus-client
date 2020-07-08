module App where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import System.Modbus.TCP
import CsvParser

type App a = ReaderT [ModData] (WriterT [ModData] Session) a

runApp :: App a -> [ModData] -> Connection ->  IO (Either ModbusException (a , [ModData]))
runApp app t conn = runExceptT $ runSession conn $ runWriterT $ runReaderT app t 