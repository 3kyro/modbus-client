{-# LANGUAGE MultiParamTypeClasses #-}
module Types.Repl
    ( Repl
    , ReplState (..)
    , ReplArg (..)
    , Command (..)
    ) where

import           Control.Concurrent               (MVar)
import           Control.Monad.IO.Class           ()
import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Word                        (Word16, Word8)
import           System.Console.Repline           (HaskelineT)


import           Types.ModData                    (ModData (..))

import           Types.Modbus                     (Config, HeartBeat (..),
                                                   RTUClient, TCPClient)

type Repl a = HaskelineT (StateT ReplState IO) a

data ReplClient = ReplTCPClient
    { unTcpClient :: TCPClient
    }
    | ReplRTUClient
    { unRTUClient :: RTUClient
    }

data ReplState = ReplState
    { replClient        :: !ReplClient
    , replConfig        :: !Config
    , replModData       :: ![ModData]
    , replUId           :: !Word8
    , replPool          :: ![MVar HeartBeat]
    , replTransactionId :: !Word16
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
