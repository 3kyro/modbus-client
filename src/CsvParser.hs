module CsvParser
  ( module CsvParser.Spec,
    module CsvParser,
  )
where

import Control.Monad (void)
import CsvParser.Spec
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Word (Word16)
import Text.Parsec 
import Text.Parsec.Text (Parser)

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
    <$> pName
    <*> pRegType
    <*> pRegAddr
    <*> pValue
    <*> pComments
    <* optional endOfLine

-- Parses a name cell
pName :: Parser String
pName = field $ (:) <$> oneOf (['A'..'Z'] ++ ['a'..'z'] ++ "_") <*> many (oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")  


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
pRegAddr :: Parser Word16
pRegAddr = field $ read <$> many1 digit

-- Parses a modbus value by associating the datatype field with
-- the correct value field
pValue :: Parser ModType
pValue = do
  dataType <- T.map toLower <$> field pText
  case T.unpack dataType of
    "float" -> ModFloat <$> pFloat 
    "word" -> ModWord <$> pMaybeWord
    _ -> fail "Parsing Error on data type"

-- Parses a floating point number
-- number can be in the form
-- leading dot: eg .24
-- no dot: eg 100
-- no fractional: eg 15.
-- usual representation: eg 10.24
-- scientific representation: eg 3.24e-12
pFloat :: Parser (Maybe Float)
pFloat = Just . read <$> float <|> nothing <?> "Float"
  where
    nothing = char ';' >> pure Nothing
    -- optional trailing scientific notation
    float = try noScientific <|> scientific
    noScientific = leadingDot <|> try noDot <|> try noFractional <|> middleDot <* char ';'
    -- number that starts with a separating dot (eg .5)
    leadingDot = char '.' *> addLeadingZero
    addLeadingZero = (++) <$> return "0." <*> many1 digit <* char ';'
    noDot = integer <* char ';'
    -- with a separating dot but no fractional part (eg 100.)
    noFractional = integer <* char '.' <* char ';'
    -- typical float representation (eg 10.52)
    middleDot = (++) <$> integer <*> fractional
    integer = (:) <$> option ' ' (char '-') <*> many1 digit
    fractional = (:) <$> char '.' <*> many1 digit
    -- parses scientific notation (eg e-23)
    scientific = (++) <$> middleDot <*> pExponent 
    pExponent = (:) <$> char 'e' <*> integer <* char ';'

-- Parses a word16, return Nothing if only a ";" is found
pMaybeWord :: Parser (Maybe Word16)
pMaybeWord = Just <$> pWord <|> nothing <?> "Word"
  where
    nothing = char ';' >> return Nothing

-- Parses a Word16
pWord :: Parser Word16
pWord = read <$> (minus <|> unsigned <?> "Parse error")
  where
    minus = char '-' *> fail "only positive numbers"
    unsigned = many1 digit <* char ';' <?> "Parse error"

-- comment is the last field, so we keep newlines unparsed
-- for consistency we don't allow semicolons in comment fields
pComments :: Parser T.Text
pComments = T.pack <$> many (noneOf ";\r\n") <* notFollowedBy (char ';')

-- Parses an inner csv field, discarding the separating semicolon
field :: Parser a -> Parser a
field p = p <* semicolon

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
