module ModDataSpec (modDataSpec) where

import Test.Hspec
import Test.QuickCheck

import Test.Aeson.GenericSpecs

import Types.ModData

modDataSpec :: IO ()
modDataSpec = hspec $ do
    goldenSpecs defaultSettings (Proxy :: Proxy ModData)
    roundtripSpecs (Proxy :: Proxy ModData)
    goldenSpecs defaultSettings (Proxy :: Proxy ModDataUpdate)
    roundtripSpecs (Proxy :: Proxy ModDataUpdate)
