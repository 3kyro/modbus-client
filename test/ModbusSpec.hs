module ModbusSpec 
       (modbusSpec) 
       where

import Test.Hspec
import Test.QuickCheck

import CsvParser
import TestHelper ()

modbusSpec :: IO ()
modbusSpec = undefined

modSessionSpec :: Spec
modSessionSpec =
  describe "Compile a valid Sesion"
    $ it "succeds on valid inputs"
    $ property prop_modSession_valid_inputs

prop_modSession_valid_inputs :: [ModData] -> Bool
prop_modSession_valid_inputs md = undefined
