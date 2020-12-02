{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.ModData (
    ModData (..),
    RegType (..),
    ModValue (..),
    NameArb (..),
    getModValueMult,
    serializeModData,
    serializeModDatum,
    ModDataUpdate (..),
    ReadWrite (..),
    setMDUModValue,
    createModData,
    WordBit,
    bitsFromBools,
bitsFromString) where

import Data.Aeson
import Data.List (foldl')
import Data.Word (Word16, Word8)
import Modbus (word16ToDouble, double2Word16, 
    Address (..),
    MBRegister (..),
    RegType (..),
    float2Word16,
    serializeRegType,
    word16ToFloat,
 )
import Test.QuickCheck (
    Arbitrary,
    arbitrary,
    elements,
    frequency,
    oneof,
 )

import Data.Bits ((.|.), Bits, setBit, testBit, zeroBits)
import Data.Range (fromSize)
import qualified Data.Text as T

---------------------------------------------------------------------------------------------------------------
-- ModData
---------------------------------------------------------------------------------------------------------------

-- The principal modbus register data struture
-- TODO #6 :: use opaque type for modName
data ModData = ModData
    { modName :: !String
    , -- Register Type
      modRegType :: !RegType
    , -- Address
      modAddress :: !Word16
    , -- Value
      modValue :: !ModValue
    , -- Unit Id
      modUid :: !Word8
    , -- Description
      modDescription :: !T.Text
    }
    deriving (Show, Eq)

instance Arbitrary ModData where
    arbitrary =
        ModData
            <$> (unNA <$> arbitrary) -- Arbitrary modName
            <*> arbitrary -- Arbitrary RegType
            <*> arbitrary -- Arbitrary Address
            <*> arbitrary -- Arbitrary ModValue
            <*> arbitrary -- Arbitrary Unit Id
            <*> arbText -- Arbitrary Description
      where
        arbText = frequency [end, rest]
        end = (1, return "")
        rest = (10, T.cons <$> descValidChar <*> arbText)
        descValidChar = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ " &éèçà$=:"

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
    toJSON md =
        object
            [ "name" .= modName md
            , "register type" .= modRegType md
            , "address" .= modAddress md
            , "register value" .= modValue md
            , "uid" .= modUid md
            , "description" .= modDescription md
            ]

instance MBRegister ModData where
    registerType = modRegType
    registerAddress md = fromSize (Address (modAddress md)) (Address $ getModValueMult $ modValue md)
    registerUID = modUid
    registerToWord16 bo md =
        case modValue md of
            ModWord Nothing -> []
            ModWord (Just v) -> [v]
            ModWordBit Nothing -> []
            ModWordBit (Just wb) -> [fromIntegral wb]
            ModFloat Nothing -> []
            ModFloat (Just v) -> float2Word16 bo v
            ModDouble Nothing -> []
            ModDouble (Just v) -> double2Word16 bo v

    registerFromWord16 _ md [] = md
    registerFromWord16 bo md vs@(x : _) =
        case modValue md of
            ModWord _ -> md{modValue = ModWord (Just x)}
            ModWordBit _ -> md{modValue = ModWordBit (Just $ fromIntegral x)}
            ModFloat _ -> md{modValue = ModFloat (word16ToFloat bo vs)}
            ModDouble _ -> md{modValue = ModDouble (word16ToDouble bo vs)}

---------------------------------------------------------------------------------------------------------------
-- ModValue
---------------------------------------------------------------------------------------------------------------

-- Modbus uses a 'big-Endian' encoding for addresses and data items.
-- This means that when a numerical quantity larger than a single byte is
-- transmitted, th most significant byte is sent first.
-- In order to transmit a 32 bit float value, two consecutive registers
-- will be used.
data ModValue
    = ModWord (Maybe Word16)
    | ModWordBit (Maybe WordBit)
    | ModFloat (Maybe Float)
    | ModDouble (Maybe Double)
    deriving (Eq)

instance Show ModValue where
    show mv =
        case mv of
            ModWord value -> showM value ++ " (Word)"
            ModWordBit value -> showM value ++ " (Word Bits)"
            ModFloat value -> showM value ++ " (Float)"
            ModDouble value -> showM value ++ " (Double)"
      where
        showM (Just x) = show x
        showM Nothing = "No current value"

instance Arbitrary ModValue where
    arbitrary =
        oneof
            [ ModWord <$> arbitrary
            , ModWordBit <$> (fmap WB <$> arbitrary)
            , ModFloat <$> arbitrary
            , ModDouble <$> arbitrary
            ]

instance ToJSON ModValue where
    toJSON mv =
        case mv of
            ModWord (Just x) ->
                object
                    [ "type" .= String "word"
                    , "value" .= x
                    ]
            ModWord Nothing ->
                object
                    [ "type" .= String "word"
                    , "value" .= Null
                    ]
            ModWordBit (Just x) ->
                object
                    [ "type" .= String "bits"
                    , "value" .= show x
                    ]
            ModWordBit Nothing ->
                object
                    [ "type" .= String "bits"
                    , "value" .= Null
                    ]
            ModFloat (Just x) ->
                object
                    [ "type" .= String "float"
                    , "value" .= x
                    ]
            ModFloat Nothing ->
                object
                    [ "type" .= String "float"
                    , "value" .= Null
                    ]
            ModDouble (Just x) ->
                object
                    [ "type" .= String "double"
                    , "value" .= x
                    ]
            ModDouble Nothing ->
                object
                    [ "type" .= String "double"
                    , "value" .= Null
                    ]

instance FromJSON ModValue where
    parseJSON (Object o) = do
        (valueType :: T.Text) <- o .: "type"
        case valueType of
            "word" -> do
                v <- o .:? "value"
                return $ ModWord v
            "bits" -> do
                v <- o .:? "value"
                return $ ModWordBit (v >>= bitsFromString)
            "float" -> do
                v <- o .:? "value"
                return $ ModFloat v
            "double" -> do
                v <- o .:? "value"
                return $ ModDouble v
            _ -> fail "Not a ModValue"
    parseJSON _ = fail "Not a ModValue"

---------------------------------------------------------------------------------------------------------------
-- ModDataUpdate
---------------------------------------------------------------------------------------------------------------

data ModDataUpdate = MDU
    { mduModData :: ModData
    , mduSelected :: Bool
    , mduRW :: ReadWrite
    }
    deriving (Show, Eq)

instance MBRegister ModDataUpdate where
    registerType = registerType . mduModData
    registerUID = registerUID . mduModData
    registerAddress = registerAddress . mduModData
    registerToWord16 order = registerToWord16 order . mduModData
    registerFromWord16 order mdu values =
        mdu{mduModData = registerFromWord16 order (mduModData mdu) values}

instance FromJSON ModDataUpdate where
    parseJSON (Object o) = do
        md <- o .: "modData"
        sl <- o .: "selected"
        rw <- o .: "rw"
        return $ MDU md sl rw
    parseJSON _ = fail "Not a ModDataUpdate"

instance ToJSON ModDataUpdate where
    toJSON mdu =
        object
            [ "modData" .= mduModData mdu
            , "selected" .= mduSelected mdu
            , "rw" .= mduRW mdu
            ]

instance Arbitrary ModDataUpdate where
    arbitrary = MDU <$> arbitrary <*> arbitrary <*> arbitrary

data ReadWrite
    = MDURead
    | MDUWrite
    deriving (Show, Eq)

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
    arbitrary = elements [MDURead, MDUWrite]

setMDUModValue :: ModDataUpdate -> ModValue -> ModDataUpdate
setMDUModValue mdu mv =
    mdu{mduModData = (mduModData mdu){modValue = mv}}

---------------------------------------------------------------------------------------------------------------
-- WordBit
---------------------------------------------------------------------------------------------------------------

-- A representation of a Word16 in bits
newtype WordBit = WB Word16
    deriving (Eq, Num, Enum, Ord, Real, Integral, Bits)

instance Show WordBit where
    show = map showB . toBits 0 . replicate 16
      where
        toBits :: Int -> [WordBit] -> [Bool]
        toBits _ [] = []
        toBits counter (x : xs) = testBit x counter : toBits (counter + 1) xs
        showB b = if b then '1' else '0'

bitsFromString :: String -> Maybe WordBit
bitsFromString str = go str zeroBits 0
  where
    go "" bit _ = Just bit
    go (c:cs) bit n
        | n > 15 = Nothing
        | c == '0'= go cs bit (n+1)
        | c == '1'= go cs bit' (n + 1)
        | otherwise = Nothing
      where
        bit' = setBit bit n .|. bit

-- Used for CSV parsing
bitsFromBools :: [Bool] -> WordBit
bitsFromBools bs = go bs zeroBits 0
  where
    go [] bit _= bit
    go (x : xs) bit n = bit' .|. go xs bit' (n + 1)
      where
        bit' = if x then setBit bit n else bit

instance Arbitrary WordBit where
    arbitrary = WB <$> arbitrary

---------------------------------------------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------------------------------------------

-- Helper type to better produce arbitrary name values
newtype NameArb = NA
    { unNA :: String
    }

instance Show NameArb where
    show (NA str) = str

-- Produce pairs of characters to make sure starting char is
-- a correct one
instance Arbitrary NameArb where
    arbitrary = go
      where
        go = NA <$> ((:) <$> validStartChars <*> tailArb)
        validStartChars = elements $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "_"
        tailArb = frequency [end, rest]
        end = (1, return "")
        rest = (7, (:) <$> nameValidChars <*> (unNA <$> go))
        nameValidChars = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_"

createModData :: RegType -> Word16 -> ModValue -> Word8 -> ModData
createModData regtype address mv uid =
    ModData
        "placeholder"
        regtype
        address
        mv
        uid
        (T.pack "placeholder description")

getModValueMult :: ModValue -> Word16
getModValueMult (ModWord _) = 1
getModValueMult (ModWordBit _) = 1
getModValueMult (ModFloat _) = 2
getModValueMult (ModDouble _) = 4

-- Serialize ModData, including the necessary header for
-- modbus table parsing
serializeModData :: [ModData] -> T.Text
serializeModData md = header `T.append` packed
  where
    header = "Name;Register Type;Register Address;Data type;Value;id;Description\n"
    packed = foldl' append "" md
    append acc md' = acc `T.append` serializeModDatum md' `T.append` "\n"

-- Serialize a single ModData
serializeModDatum :: ModData -> T.Text
serializeModDatum md =
    T.pack
        ( modName md ++ ";"
            ++ serializeRegType (modRegType md)
            ++ ";"
            ++ show (modAddress md)
            ++ ";"
            ++ serializeModValue (modValue md)
            ++ show (modUid md)
            ++ ";"
        )
        `T.append` modDescription md

serializeModValue :: ModValue -> String
serializeModValue mt =
    case mt of
        ModWord mv -> "word;" ++ serMaybe mv ++ ";"
        ModWordBit mv -> "bits;" ++ serMaybe mv ++ ";"
        ModFloat fl -> "float;" ++ serMaybe fl ++ ";"
        ModDouble dl -> "double;" ++ serMaybe dl ++ ";"
  where
    serMaybe (Just x) = show x
    serMaybe Nothing = ""
