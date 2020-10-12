{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
module Types.Repl
    ( Repl
    , ReplState (..)
    , ReplArg (..)
    , Command (..)
    , ReadRegFun
    ) where

import           Control.Concurrent               (MVar)


import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Word                        (Word16, Word8)
import           System.Console.Repline           (HaskelineT)


import           Types.ModData                    (ModData (..))


import           Control.Exception.Safe           (throwIO)
import           Control.Monad.Catch
import           Control.Monad.State.Strict       (lift)
import           Data.Range                       (Range)
import           Data.Tagged                      (Tagged)
import           Modbus                     (TID, Address, ByteOrder, Client,
                                                   HeartBeat (..),
                                                   ModbusProtocol, Session,
                                                   TransactionInfo, Worker)
import Control.Concurrent.STM (TVar)

type Repl = HaskelineT (StateT ReplState IO)

instance MonadMask Repl where
    mask = mask
    uninterruptibleMask = uninterruptibleMask
    generalBracket = generalBracket
instance MonadThrow Repl where
    throwM = lift . throwIO
instance MonadCatch Repl where
    catch = catch

data ReplState = ReplState
    { replClient        :: !(MVar Client)
    , replProtocol      :: !ModbusProtocol
    , replDirectWorker  :: !(Worker IO)
    , replBatchWorker   :: !(Worker IO)
    , replByteOrder     :: !ByteOrder
    , replModData       :: ![ModData]
    , replUId           :: !Word8
    , replPool          :: ![HeartBeat]
    , replTransactionId :: !(TVar TID)
    }

-- Defines the type of an argument in certain repl commands
data ReplArg = ReplName String
    | ReplAddr Word16
    deriving Show

data Command = ReadInputRegistersWord
    | ReadInputRegistersFloat
    | ReadHoldingRegistersWord
    | ReadHoldingRegistersFloat
    | WriteRegistersWord
    | WriteRegistersFloat
    | Read
    | Write
    | Heartbeat
    | StopHeartbeat
    | ListHeartbeat
    | Import
    | Export
    | Id
    | CommandNotFound
    deriving (Eq, Show)

type ReadRegFun a = TransactionInfo -> Range Address -> Tagged a (Session IO [Word16])
