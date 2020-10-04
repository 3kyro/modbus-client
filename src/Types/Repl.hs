{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Control.Exception.Safe           (MonadThrow)
import           Data.Range                       (Range)
import           Data.Tagged                      (Tagged)
import           Types.Modbus                     (Address, ByteOrder, Config,
                                                   HeartBeat (..), RTUClient,
                                                   Session, TCPClient,
                                                   TransactionInfo)


type Repl a b = HaskelineT (StateT (ReplState a) IO) b

data ReplState a = ReplState
    { replClient        :: MVar a
    , replModData       :: ![ModData]
    , replUId           :: !Word8
    , replPool          :: ![MVar HeartBeat]
    , replTransactionId :: !Word16
    , replByteOrder     :: !ByteOrder
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
