module ModbusSpec 
       (modbusSpec)
       where

import Test.Hspec ( hspec, describe, it, Spec )
import Test.QuickCheck
import Test.Aeson.GenericSpecs

import Modbus
import TestHelper ()

modbusSpec :: IO ()
modbusSpec = hspec $ do
    byteOrderConversionsSpec
    roundtripAndGoldenSpecs (Proxy :: Proxy SerialSettings)
    roundtripAndGoldenSpecs (Proxy :: Proxy BaudRate)
    roundtripAndGoldenSpecs (Proxy :: Proxy StopBits)
    roundtripAndGoldenSpecs (Proxy :: Proxy Parity)

byteOrderConversionsSpec :: Spec
byteOrderConversionsSpec = describe "Convert data types using ByteOrder" $ do
    it "Converts to/from Float" $ property propConvertFloat
    it "Converts to Float in Big Endian" $
        word16ToFloat BE [17492, 32768] == Just 850.0
    it "Converts to Float in Little Endian" $
        word16ToFloat LE [32768, 17492] == Just 850.0


propConvertFloat :: ByteOrder -> Float -> Bool
propConvertFloat bo float =
    Just float == word16ToFloat bo (float2Word16 bo float)


