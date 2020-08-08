-- |
-- Module : CsvParser.Spec
-- Description: Specifications for the csv files parsed by modbus-serve
--
-- Modbus-serve can use input in the form of a csv file. This module defines the specifications of this csv file.
-- - The first line of the document contains field descriptions and will be ignored by the parser
-- - Cells are delimited by a semicolon
-- - Decimal nummbers are dot seperated (eg 1.5)
-- - Text fields cannot contain newline characters
-- - The name field must start with either an alphabetic character or an underscore
-- - The name field can contain only alphanumeric characters and underscore.
-- - Register addresses above 65535 are truncated
-- - Order of fields is the following:
--   Name;Register Type;Register Address;Data Type; Value;Descriprtion
module CsvParser.Spec
    ( 
      ModData (..)
    , RegType (..)
    , ModType (..)
    , ByteOrder (..)
    , modData
    , NameArb (..)
    , getModTypeMult
    )
    where

import Data.Word (Word16)
import Test.QuickCheck 
    ( 
      Arbitrary
    , arbitrary
    , oneof
    , elements
    , frequency
    )

import qualified Data.Text as T

-- Modbus Register type:
-- Discrete Input, single bit, read only
-- Coil Single bit, read / write
-- Input Register, 16-bit word, read only
-- Holding Register, 16-bit word, read / write
data RegType 
    = DiscreteInput 
    | Coil 
    | InputRegister 
    | HoldingRegister
    deriving (Show, Eq)

data ModData = ModData
    { 
      modName           :: !String  -- Variable name
    , modRegType        :: !RegType -- Type (Holdin Register - Input Register)
    , modAddress        :: !Word16  -- Address
    , modValue          :: !ModType -- Value (incluting value type)
    , modDescription    :: !T.Text  -- Description
    }
    deriving (Show, Eq)

-- Modbus uses a 'big-Endian' encoding for addresses and data items.
-- This means that when a numerical quantity larger than a single byte is 
-- transmitted, th most significant byte is sent first.
-- In order to transmit a 32 bit float value, two consecutive registers
-- will be used. 
data ModType 
    = ModWord   (Maybe Word16) 
    | ModFloat  (Maybe Float)
    deriving (Show, Eq)

-- Byte order of data types
-- Eg: when receiving two two-byte words AB and CD
-- LE   - AB CD
-- BE   - CD AB
-- LESW - BA DC
-- BESW - DC BA       
data ByteOrder
    = LE    -- Little Endian
    | BE    -- Big Endian
    | LESW  -- Little Endiann, byte swap for each word
    | BESW  -- Big Endian, byte swap for each word

    deriving (Show, Read, Eq)

-- ModData constructor
modData :: String -> RegType -> Word16 -> ModType -> T.Text -> ModData
modData n rt r v c = 
    ModData
    { 
      modName = n      
    , modRegType = rt
    , modAddress = r
    , modValue = v
    , modDescription = c
    }

instance Arbitrary ModType where
  arbitrary = oneof [ModWord <$> arbitrary, ModFloat <$> arbitrary]

instance Arbitrary ModData where
  arbitrary =
    modData <$> (unNA <$> arbitrary) <*> arbitrary <*> arbitrary <*> arbitrary <*> arbText
    where
      arbText = frequency [end,rest]
      end = (1, return (T.pack ""))
      rest = (10, T.cons <$> descValidChar <*> arbText)
      descValidChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " &éèçà$=:"

instance Arbitrary RegType where
  arbitrary = elements [DiscreteInput, Coil, InputRegister, HoldingRegister]

-- Helper type to better produce arbitrary name values
newtype NameArb = NA  {
    unNA :: String
}

instance Show NameArb where
    show (NA str) = str

-- Produce pairs of characters to make sure starting char is 
-- a correct one
instance Arbitrary NameArb where
    arbitrary = go 
        where
      go = NA <$> ((:) <$> validStartChars <*> tailArb)      
      validStartChars = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ "_"
      tailArb = frequency [end, rest]
      end = (1, return "")
      rest = (7, (:) <$> nameValidChars <*> (unNA <$> go))
      nameValidChars = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"


getModTypeMult :: ModType -> Word16
getModTypeMult (ModWord _) = 1
getModTypeMult (ModFloat _) = 2