module TestHelper where

import Types
import qualified Data.Text as T

class TestShow a where
  tShow :: a -> String

instance TestShow ModValue where
  tShow (ModWord (Just x)) = "word;" ++ show x
  tShow (ModWord Nothing) = "word;"
  tShow (ModFloat (Just x)) = "float;" ++ show x
  tShow (ModFloat Nothing) = "float;"

instance TestShow RegType where
  tShow DiscreteInput = "Discrete Input"
  tShow Coil = "Coil"
  tShow InputRegister = "Input Register"
  tShow HoldingRegister = "Holding Register"

instance TestShow ModData where
  tShow (ModData nm rt reg val uid com) =
         nm
      ++ ";"
      ++ tShow rt
      ++ ";"
      ++ show reg
      ++ ";"
      ++ tShow val
      ++ ";"
      ++ show uid
      ++ ";"
      ++ T.unpack com

