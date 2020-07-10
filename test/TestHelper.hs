module TestHelper where

import CsvParser
import qualified Data.Text as T
import System.Modbus.TCP (RegAddress (..))
import Test.QuickCheck

instance Arbitrary ModType where
  arbitrary = oneof [ModWord <$> arbitrary, ModFloat <$> arbitrary]

instance Arbitrary ModData where
  arbitrary =
    modData <$> arbText <*> arbitrary <*> arbitrary <*> arbitrary <*> arbText
    where
      arbText = oneof [return (T.pack ""), T.cons <$> validChar <*> arbText]
      validChar = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "&éèçà$=:"

instance Arbitrary RegType where
  arbitrary = elements [DiscreteInput, Coil, InputRegister, HoldingRegister]

instance Arbitrary RegAddress where
  arbitrary = RegAddress <$> arbitrary

class TestShow a where
  tShow :: a -> String

instance TestShow ModType where
  tShow (ModWord (Just x)) = "word;" ++ show x
  tShow (ModWord Nothing) = "word;"
  tShow (ModFloat (Just x)) = "float;" ++ show x
  tShow (ModFloat Nothing) = "float;"

instance TestShow RegType where
  tShow DiscreteInput = "Discrete Input"
  tShow Coil = "Coil"
  tShow InputRegister = "Input Register"
  tShow HoldingRegister = "Holding Register"

instance TestShow RegAddress where
  tShow = show . unRegAddress

instance TestShow ModData where
  tShow (ModData desc rt reg val com) =
    T.unpack desc
      ++ ";"
      ++ tShow rt
      ++ ";"
      ++ tShow reg
      ++ ";"
      ++ tShow val
      ++ ";"
      ++ T.unpack com