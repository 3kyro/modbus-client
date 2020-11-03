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


import           Control.Concurrent.STM           (TVar)
import           Data.Range                       (Range)
import           Data.Tagged                      (Tagged)
import           Modbus                           (RTUWorker, Address, ByteOrder, Client,
                                                   HeartBeat (..),
                                                   ModbusProtocol, TCPSession, TID,
                                                   TransactionInfo, TCPWorker)

type Repl = HaskelineT (StateT ReplState IO)

data ReplState = ReplState
    { replClient        :: !(MVar Client)
    , replProtocol      :: !ModbusProtocol
    , replTCPDirectWorker  :: !(TCPWorker IO)
    , replTCPBatchWorker   :: !(TCPWorker IO)
    , replRTUDirectWorker  :: !(RTUWorker IO)
    , replRTUBatchWorker   :: !(RTUWorker IO)
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
    | ReadInputRegistersBits
    | ReadInputRegistersFloat
    | ReadHoldingRegistersWord
    | ReadHoldingRegistersBits
    | ReadHoldingRegistersFloat
    | WriteRegistersWord
    | WriteRegistersBits
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

type ReadRegFun a = TransactionInfo -> Range Address -> Tagged a (TCPSession IO [Word16])
