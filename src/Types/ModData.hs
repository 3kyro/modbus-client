{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.ModData
    ( ModData (..)
    , RegType (..)
    , ModValue (..)
    , ByteOrder (..)
    , NameArb (..)
    , getModValueMult
    , serializeModData
    , ModDataUpdate (..)
    , ReadWrite (..)
    ,setMDUModValue)
    where

import Data.Aeson
import Data.Word (Word8, Word16)
import Data.List (foldl')
import Test.QuickCheck
    ( Arbitrary
    , arbitrary
    , oneof
    , elements
    , frequency
    )
import Types.Modbus (RegType (..), serializeRegType)

import qualified Data.Text as T

---------------------------------------------------------------------------------------------------------------
-- ModData
---------------------------------------------------------------------------------------------------------------

-- The principal modbus register data struture
data ModData = ModData
    { modName           :: !String      -- Variable name
    , modRegType        :: !RegType     -- Register Type
    , modAddress        :: !Word16      -- Address
    , modValue          :: !ModValue    -- Value
    , modUid            :: !Word8       -- Unit Id
    , modDescription    :: !T.Text      -- Description
    } deriving (Show, Eq)

instance Arbitrary ModData where
    arbitrary
        = ModData
        <$> (unNA <$> arbitrary)    -- Arbitrary modName
        <*> arbitrary               -- Arbitrary RegType
        <*> arbitrary               -- Arbitrary Address
        <*> arbitrary               -- Arbitrary ModValue
        <*> arbitrary               -- Arbitrary Unit Id
        <*> arbText                 -- Arbitrary Description
      where
        arbText = frequency [end,rest]
        end = (1, return "")
        rest = (10, T.cons <$> descValidChar <*> arbText)
        descValidChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " &éèçà$=:"

instance FromJSON ModData where
    parseJSON (Object o) = do
        name <- o .: "name"
        regType <- o .: "register type"
        addr <- o .: "address"
        value <- o .: "register value"
        uid <- o .: "uid"
        desc <- o .: "description"
        return $ ModData name regType addr value uid desc
    parseJSON _ = fail "Not a ModData"


instance ToJSON ModData where
    toJSON md = object
        [ "name" .= modName md
        , "register type" .= modRegType md
        , "address" .= modAddress md
        , "register value" .= modValue md
        , "uid" .= modUid md
        , "description" .= modDescription md
        ]

---------------------------------------------------------------------------------------------------------------
-- ModValue
---------------------------------------------------------------------------------------------------------------


-- Modbus uses a 'big-Endian' encoding for addresses and data items.
-- This means that when a numerical quantity larger than a single byte is
-- transmitted, th most significant byte is sent first.
-- In order to transmit a 32 bit float value, two consecutive registers
-- will be used.
data ModValue
    = ModWord   (Maybe Word16)
    | ModFloat  (Maybe Float)
    deriving (Eq)

instance Show ModValue where
    show mv =
        case mv of
            ModWord value -> showM value ++ " (Word)"
            ModFloat value -> showM value ++ " (Float)"
      where
          showM (Just x) = show x
          showM Nothing = "No current value"

instance Arbitrary ModValue where
    arbitrary = oneof [ModWord <$> arbitrary, ModFloat <$> arbitrary]

instance ToJSON ModValue where
    toJSON mv =
        case mv of
            ModWord (Just x) -> object
                [ "type" .= String "word"
                , "value" .= x
                ]
            ModWord Nothing -> object
                ["type" .= String "word"
                , "value" .= Null
                ]
            ModFloat (Just x) -> object
                [ "type" .= String "float"
                , "value" .= x
                ]
            ModFloat Nothing -> object
                ["type" .= String "float"
                , "value" .= Null
                ]

instance FromJSON ModValue where
    parseJSON (Object o) = do
        (valueType :: T.Text) <- o .: "type"
        case valueType of
            "word" -> do
                v <- o .:? "value"
                return $ ModWord v
            "float" -> do
                v <- o .:? "value"
                return $ ModFloat v
    parseJSON _ = fail "Not a ModValue"

---------------------------------------------------------------------------------------------------------------
-- ModDataUpdate
---------------------------------------------------------------------------------------------------------------

data ModDataUpdate = MDU
    { mduModData :: ModData
    , mduSelected :: Bool
    , mduRW :: ReadWrite
    }
    deriving (Show)

instance FromJSON ModDataUpdate where
    parseJSON (Object o) = do
        md <- o .: "modData"
        sl <- o .: "selected"
        rw <- o .: "rw"
        return $ MDU md sl rw

instance ToJSON ModDataUpdate where
    toJSON mdu = object
        [ "modData" .= mduModData mdu
        , "selected" .= mduSelected mdu
        , "rw" .= mduRW mdu
        ]

instance Arbitrary ModDataUpdate where
    arbitrary = MDU <$> arbitrary <*> arbitrary <*> arbitrary

data ReadWrite
    = MDURead
    | MDUWrite
    deriving (Show)

instance FromJSON ReadWrite where
    parseJSON (String s) =
        case s of
            "read" -> return MDURead
            "write" -> return MDUWrite
            _ -> fail "Not a ReadWrite"
    parseJSON _ = fail "Not a ReadWrite"

instance ToJSON ReadWrite where
    toJSON rw =
        case rw of
            MDURead -> String "read"
            MDUWrite -> String "write"

instance Arbitrary ReadWrite where
    arbitrary = elements [MDURead , MDUWrite]

setMDUModValue :: ModDataUpdate -> ModValue -> ModDataUpdate
setMDUModValue mdu mv =
    mdu { mduModData = (mduModData mdu) { modValue = mv}}


---------------------------------------------------------------------------------------------------------------
-- ByteOrder
---------------------------------------------------------------------------------------------------------------

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

---------------------------------------------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------------------------------------------

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


getModValueMult :: ModValue -> Word16
getModValueMult (ModWord _) = 1
getModValueMult (ModFloat _) = 2

-- Serialize ModData, including the necessary header for
-- modbus table parsing
serializeModData :: [ModData] -> T.Text
serializeModData md = header `T.append` packed
  where
    header = "Name;Register Type;Register Address;Data type;Value;Description\n"
    packed = foldl' append "" md
    append acc md' = acc `T.append` serializeModDatum md' `T.append` "\n"

-- Serialize a single ModData
serializeModDatum :: ModData -> T.Text
serializeModDatum md =
    T.pack
        (  modName md ++ ";"
        ++ serializeRegType (modRegType md) ++ ";"
        ++ show (modAddress md) ++ ";"
        ++ serializeModValue (modValue md)
        )
    `T.append` modDescription md


serializeModValue :: ModValue -> String
serializeModValue mt =
    case mt of
        ModWord mv  -> "word;" ++ serMaybe mv ++ ";"
        ModFloat fl -> "float;" ++ serMaybe fl ++ ";"
  where
    serMaybe (Just x) = show x
    serMaybe Nothing = ""



