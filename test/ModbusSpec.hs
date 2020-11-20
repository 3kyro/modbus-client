module ModbusSpec (modbusSpec) where

import Test.Aeson.GenericSpecs
import Test.Hspec (Spec, describe, hspec, it)
import Test.QuickCheck

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Modbus
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import TestHelper ()
import Data.Range
import Data.Binary (Word16)
import Data.List (foldl')

modbusSpec :: IO ()
modbusSpec = hspec $ do
    wordOrderConversionsSpec
    roundtripAndGoldenSpecs (Proxy :: Proxy ModbusProtocol)
    roundtripAndGoldenSpecs (Proxy :: Proxy WordOrder)
    roundtripAndGoldenSpecs (Proxy :: Proxy SerialSettings)
    roundtripAndGoldenSpecs (Proxy :: Proxy BaudRate)
    roundtripAndGoldenSpecs (Proxy :: Proxy StopBits)
    roundtripAndGoldenSpecs (Proxy :: Proxy Parity)
    roundtripAndGoldenSpecs (Proxy :: Proxy HeartbeatType)
    incrementTIDspec
    getFunctionAccSpec

wordOrderConversionsSpec :: Spec
wordOrderConversionsSpec = describe "Convert data types using WordOrder" $ do
    it "Converts to/from Float" $ property propConvertFloat
    it "Converts to Float in Big Endian" $
        word16ToFloat BE [17492, 32768] == Just 850.0
    it "Converts to Float in Little Endian" $
        word16ToFloat LE [32768, 17492] == Just 850.0

propConvertFloat :: WordOrder -> Float -> Bool
propConvertFloat bo float =
    Just float == word16ToFloat bo (float2Word16 bo float)

incrementTIDspec :: Spec
incrementTIDspec =
    describe "Transaction Info tests" $
        it "atomically increments a TID" $ property propIncrementTID

propIncrementTID :: Property
propIncrementTID = monadicIO $ do
    let repeats = 100
    tid <- run initTID
    run $ mapConcurrently (\_ -> forkIO $ void $ getNewTID tid) (replicate (fromIntegral repeats) 0)
    incremented <- run $ getNewTID tid
    assert $ unTID incremented == repeats + 1

getFunctionAccSpec :: Spec
getFunctionAccSpec =
    describe "propGetFunctionAcc" $
        it "Correctly assigns HeartbeatType functions and values" $
            property propGetFunctionAcc

propGetFunctionAcc :: HeartbeatType -> Word16 -> Bool
propGetFunctionAcc hbtp n =
    let
        (f, acc) = getFunctionAcc hbtp
        accs = take (fromIntegral n) $ iterate f acc
    in
        case hbtp of
            Increment -> foldl' (\b w -> b && f w == w + 1) True accs
            Pulse value -> all (== value) accs
            Alternate (low, hi) -> all (\e -> e == low || e == hi) accs
            Range range -> foldl'
                (\b acc ->
                    b
                    && if acc < end range then f acc == (acc + 1) else f acc == begin range
                ) True accs