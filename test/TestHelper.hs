module TestHelper where

import Types
import qualified Data.Text as T
import System.Random (randomRs, randomRIO, newStdGen)
class TestShow a where
  tShow :: a -> String

instance TestShow ModValue where
  tShow (ModWord (Just x)) = "word;" ++ show x
  tShow (ModWord Nothing) = "word;"
  tShow (ModWordBit (Just x)) = "bits;" ++ show x
  tShow (ModWordBit Nothing) = "bits;"
  tShow (ModFloat (Just x)) = "float;" ++ show x
  tShow (ModFloat Nothing) = "float;"
  tShow (ModDouble (Just x)) = "double;" ++ show x
  tShow (ModDouble Nothing) = "double;"

instance TestShow RegType where
--   tShow DiscreteInput = "Discrete Input"
--   tShow Coil = "Coil"
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

-- picks a random number of elements from the list
pickFromList :: [a] -> IO [a]
pickFromList [] = pure []
pickFromList xs = do
    num <- randomRIO (0, len - 1)
    gen <- newStdGen
    let picks = take num $ randomRs (0, len -1) gen
    pure $ map (xs !!) picks
  where len = length xs