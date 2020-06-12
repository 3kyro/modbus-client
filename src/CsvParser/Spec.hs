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

data ModDatum = ModDatum {
    description :: T.Text,
    function :: ModFunction,
    register :: Word16,
    value :: ModType,
    comments :: T.Text
} deriving (Show)


data ModType = ModWord Word16 | ModFloat Float
    deriving (Show)

data ModFunction = ReadInput | ReadMultHolding | WriteSingleHolding | WriteMultHolding
    deriving (Show)

modData :: T.Text -> ModFunction -> Word16 -> ModType -> T.Text -> ModDatum
modData d f r v c = ModDatum { description = d
                             , function    = f
                             , register    = r
                             , value       = v
                             , comments    = c
                             }
