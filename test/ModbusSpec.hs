module ModbusSpec 
       (modbusSpec)
       where

import Test.Hspec
import Test.QuickCheck

import Modbus
import TestHelper ()

modbusSpec :: IO ()
modbusSpec = hspec byteOrderConversionsSpec

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

