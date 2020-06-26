{-|
Module : CsvParser.Spec
Description: Specifications for the csv files parsed by modbus-serve

Modbus-serve can use input in the form of a csv file. This module defines the specifications of this csv file. 
- The first line of the document contains field descriptions and will be ignored by the parser
- Cells are delimited by a semicolon
- Decimal nummbers are dot seperated (eg 1.5)
- Text fields cannot contain newline characters
- For now only functions 3,4,6,16 are supported
- Register addresses above 65535 are truncated
- Order of fields is the following:
    Description;Function;Register;Data Type; Value;Comments
-}
module CsvParser.Spec where

import           Data.Text                     as T
import           Data.Word                      ( Word16 )

data ModData = ModData {
    description :: T.Text,
    function :: ModFunction,
    register :: Word16,
    value :: ModType,
    comments :: T.Text
} deriving (Show,Eq)

data ModType = ModWord (Maybe Word16) | ModFloat (Maybe Float)
    deriving (Show,Eq)

data ModFunction = ReadInput | ReadMultHolding | WriteSingleHolding | WriteMultHolding
    deriving (Show, Eq)

isFnRead :: ModFunction -> Bool
isFnRead fn | fn == ReadInput || fn == ReadMultHolding = True
            | otherwise = False

modData :: T.Text -> ModFunction -> Word16 -> ModType -> T.Text -> ModData
modData d f r v c = ModData { description = d
                             , function    = f
                             , register    = r
                             , value       = v
                             , comments    = c
                             }
