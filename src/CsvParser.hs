module CsvParser
  ( module CsvParser.Spec,
    module CsvParser,
  )
where

import Control.Monad (void)
import Data.Char (toLower)
import Data.Word (Word16)
import Text.Parsec 
import Text.Parsec.Text (Parser)

import qualified Data.Text as T

import CsvParser.Spec


-- test runner, mainly used for testing
testCSVParser :: Parser a -> String -> Either ParseError a
testCSVParser p s = parse p "" $ T.pack s

-- run a pCSV parser
runpCSV :: T.Text -> Either ParseError [ModData]
runpCSV = parse pCSV "" 

-- Parses a CSV text, ignoring the first line that will be used for describing
-- the fields
pCSV :: Parser [ModData]
pCSV = pLine *> many pModData <* eof

-- Parses a ModData
pModData :: Parser ModData
pModData =
  modData
    <$> field pName
    <*> pRegType
    <*> field pWord16
    <*> pValue
    <*> pComments
    <* optional endOfLine

-- Parses a name cell
pName :: Parser String
pName = (:) <$> oneOf (['A'..'Z'] ++ ['a'..'z'] ++ "_") <*> many (oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")  

-- Parses a regiter type
pRegType :: Parser RegType
pRegType = do
  rt <- T.map toLower <$> field pText
  case T.unpack rt of
    "discrete input" -> return DiscreteInput
    "coil" -> return Coil
    "input register" -> return InputRegister
    "holding register" -> return HoldingRegister
    _ -> fail "Parse error on register type"

-- Parses a register address
pWord16 :: Parser Word16
pWord16 = read <$> many1 digit

-- Parses a modbus value by associating the datatype field with
-- the correct value field
pValue :: Parser ModType
pValue = do
  dataType <- T.map toLower <$> field pText
  case T.unpack dataType of
    "float" -> ModFloat <$> pMaybeFloat 
    "word" -> ModWord <$> pMaybeWord
    _ -> fail "Parsing Error on data type"

-- Parses a float field, returns Nothing if field is empty
pMaybeFloat :: Parser (Maybe Float)
pMaybeFloat = Just <$> combinations <|> nothing 
  where
    combinations = field pFloatLeadingDot <|> noDot <|> noFractional <|> dotted <|> scientific
    noDot = try $ fromIntegral <$> field pInt
    noFractional = try $ field pFloatNoFractional
    dotted = try $ field pFloatDotted
    scientific = try $ field pFloatScientific
    nothing = char ';' >> pure Nothing

-- Parses a floating point number
-- number can be in the form
-- leading dot: eg .24
-- no dot: eg 100
-- no fractional: eg 15.
-- usual representation: eg 10.24
-- scientific representation: eg 3.24e-12
pFloat :: Parser Float
pFloat = 
        pFloatLeadingDot 
    <|> try (only $ fromIntegral <$> pInt) 
    <|> try (only pFloatNoFractional) 
    <|> try (only pFloatDotted) 
    <|> pFloatScientific

-- Parses a word16, returns Nothing if field is empty
pMaybeWord :: Parser (Maybe Word16)
pMaybeWord = Just <$> field pWord16 <|> nothing <?> "Word"
  where
    nothing = char ';' >> return Nothing

-- Parses a Word
pWord :: Parser Word
pWord = read <$> many1 digit

-- PArses an Int
pInt :: Parser Int
pInt = read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)

-- Parses a float in leading dot format (eg .2)
pFloatLeadingDot :: Parser Float
pFloatLeadingDot = read <$> (char '.' *> addLeadingZero)
  where 
    addLeadingZero = (++) <$> return "0." <*> many1 digit

-- Parses a float when no fractional is given
pFloatNoFractional :: Parser Float
pFloatNoFractional = read <$> show <$> pInt <* char '.'

-- Parses a float in the normal (integer dot fractional) representation
pFloatDotted :: Parser Float
pFloatDotted = read <$> pFloatDottedRaw

pFloatDottedRaw :: Parser String
pFloatDottedRaw = (:) <$> option ' ' (char '-') <*> float
  where 
      float = (++) <$> integer <*> fractional
      integer = many1 digit
      fractional = (:) <$> char '.' <*> many1 digit

-- Parses a float in scientific format eg 1.3e-2
pFloatScientific :: Parser Float
pFloatScientific = read <$> ((++) <$> dotted <*> expo)
  where
      dotted = pFloatDottedRaw
      expo = (:) <$> char 'e' <*> integer
      integer = (:) <$> option ' ' (char '-') <*> many1 digit
      
-- comment is the last field, so we keep newlines unparsed
-- for consistency we don't allow semicolons in comment fields
pComments :: Parser T.Text
pComments = T.pack <$> many (noneOf ";\r\n") <* notFollowedBy (char ';')

-- Parses an inner csv field, discarding the separating semicolon
field :: Parser a -> Parser a
field p = p <* semicolon

only :: Parser a -> Parser a
only p = p <* eof

semicolon :: Parser Char
semicolon = char ';'

-- Parses text fields
-- Text fields cannot contain newline characters
pText :: Parser T.Text
pText = T.pack <$> many (noneOf ";\r\n")

pLine :: Parser T.Text
pLine = T.pack <$> manyTill anyChar endOfLine

discardField :: Parser ()
discardField = void $ field $ manyTill anyChar semicolon
