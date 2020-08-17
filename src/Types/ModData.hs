module Types.ModData
    (
      ModData (..)
    , RegType (..)
    , ModValue (..)
    , ByteOrder (..)
    , modData
    , NameArb (..)
    , getModValueMult
    , serializeModData
    )
    where
        
import Data.Word (Word16)
import Data.List (foldl')
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
    deriving (Eq)

instance Show RegType where
    show DiscreteInput = "Discrete Input"
    show Coil = "Coil"
    show InputRegister = "Input Register"
    show HoldingRegister = "Holding Register"

data ModData = ModData
    { 
      modName           :: !String  -- Variable name
    , modRegType        :: !RegType -- Type (Holdin Register - Input Register)
    , modAddress        :: !Word16  -- Address
    , modValue          :: !ModValue -- Value (incluting value type)
    , modDescription    :: !T.Text  -- Description
    }
    deriving (Show, Eq)



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
modData :: String -> RegType -> Word16 -> ModValue -> T.Text -> ModData
modData n rt r v c = 
    ModData
    { 
      modName = n      
    , modRegType = rt
    , modAddress = r
    , modValue = v
    , modDescription = c
    }

instance Arbitrary ModValue where
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


getModValueMult :: ModValue -> Word16
getModValueMult (ModWord _) = 1
getModValueMult (ModFloat _) = 2

-- Serialize ModData, including the necessary header for
-- modbus table parsing
serializeModData :: [ModData] -> T.Text
serializeModData md = T.append header packed
  where 
      header = T.pack "Name;Register Type;Register Address;Data type;Value;Description\n"
      packed = foldl' append (T.pack "") md
      append acc md' = T.append acc (serializeModDatum md') `T.append` (T.pack "\n")

-- Serialize a single ModData
serializeModDatum :: ModData -> T.Text
serializeModDatum md = 
    T.pack 
        (
        modName md ++ ";"
        ++ serializeRegType (modRegType md) ++ ";"
        ++ show (modAddress md) ++ ";"
        ++ serializeModValue (modValue md) 
        )
    `T.append` (modDescription md) 

serializeRegType :: RegType -> String
serializeRegType rt = 
    case rt of
        DiscreteInput -> "discrete input"
        Coil -> "coil"
        InputRegister -> "input register"
        HoldingRegister -> "holding register"

serializeModValue :: ModValue -> String
serializeModValue mt = 
    case mt of
        ModWord mv -> "word;" ++ serMaybe mv ++ ";"
        ModFloat fl -> "float;" ++ serMaybe fl ++ ";"
  where
      serMaybe (Just x) = show x
      serMaybe Nothing = ""
