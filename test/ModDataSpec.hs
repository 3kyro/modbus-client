{-# LANGUAGE OverloadedStrings   #-}
module ModDataSpec (modDataSpec) where

import Test.Hspec

import Test.Aeson.GenericSpecs

import Types.ModData
import CsvParser (pModData, testCSVParser)
import qualified Data.Text as T
import Test.QuickCheck (property)

modDataSpec :: IO ()
modDataSpec = hspec $ do
    goldenSpecs defaultSettings (Proxy :: Proxy ModData)
    roundtripSpecs (Proxy :: Proxy ModData)
    goldenSpecs defaultSettings (Proxy :: Proxy ModDataUpdate)
    roundtripSpecs (Proxy :: Proxy ModDataUpdate)
    serializeSpec

serializeSpec :: Spec
serializeSpec = describe "Serialize a Modbus Register" $
    it "correctly performs roundtrip serialization" $
        property $ \md ->
            Right (md :: ModData) == testCSVParser pModData (T.unpack $ serializeModDatum md)