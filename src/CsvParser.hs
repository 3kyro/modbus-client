{-|
Module : CsvParser
Description: Defines the parser used for csv files


-}
module CsvParser
    ( module CsvParser.Spec
    , module CsvParser
    )
where

import           Text.Parsec.Text               ( Parser )
import           Text.Parsec                    ( char
                                                , many
                                                , noneOf
                                                , digit
                                                , many1
                                                , try
                                                , eof
                                                , anyChar
                                                , manyTill
                                                , endOfLine
                                                , optional
                                                , parse
                                                , option
                                                , notFollowedBy
                                                , (<?>)
                                                , ParseError
                                                )
import           Control.Applicative            ( (<|>) )
import qualified Data.Text                     as T
import           CsvParser.Spec
import           Data.Word                      ( Word16 )
import           Data.Char                      ( toLower )
import           Control.Monad                  ( void )

testCSVParser :: Parser a -> String -> Either ParseError a
testCSVParser p s = parse p "" $ T.pack s

pCSV :: Parser [ModDatum]
pCSV = pLine *> many pModDatum <* eof

-- Parses a ModDatum
pModDatum :: Parser ModDatum
pModDatum =
    modData
        <$> pDescription
        <*> pFunction
        <*> pRegister
        <*> pValue
        <*> pComments
        <*  optional endOfLine

pDescription :: Parser T.Text
pDescription = field pText

pFunction :: Parser ModFunction
pFunction = do
    code <- field $ many digit
    case code of
        "3"  -> return ReadInput
        "4"  -> return ReadMultHolding
        "6"  -> return WriteSingleHolding
        "16" -> return WriteMultHolding
        _    -> fail "Parse error on Modbus function call"

pRegister :: Parser Word16
pRegister = field $ read <$> many digit

pValue :: Parser ModType
pValue = do
    dataType <- T.map toLower <$> field pText
    case T.unpack dataType of
        "float" -> field $ ModFloat <$> pFloat
        "word"  -> field $ ModWord <$> pWord
        _       -> fail "Parsing Error on data type"

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
    nothing        = eof >> return Nothing
    -- optional trailing scientific notation  
    float          = (++) <$> noScientific <*> option "" scientific
    -- optional leading dot (eg .2)
    noScientific   = leadingDot <|> try noDot <|> try noFractional <|> middleDot
    -- number that starts with a separating dot (eg .5)
    leadingDot     = char '.' *> addLeadingZero
    addLeadingZero = (++) <$> return "0." <*> many1 digit
    -- parses a
    noDot          = integer <* notFollowedBy anyChar
    -- with a separating dot but no fractional part (eg 100.)
    noFractional   = integer <* char '.' <* notFollowedBy anyChar
    -- typical float representation (eg 10.52)
    middleDot      = (++) <$> integer <*> fractional
    integer        = (:) <$> option ' ' (char '-') <*> many1 digit
    fractional     = (:) <$> char '.' <*> many1 digit
    -- parses scientific notation (eg e-23)
    scientific     = (:) <$> char 'e' <*> integer


-- Parses a word16 
pWord :: Parser (Maybe Word16)
pWord = Just . read <$> word <|> nothing <?> "Word"
  where
    word     = minus <|> unsigned <?> "Parse error"
    minus    = char '-' *> fail "only positive numbers"
    unsigned = many1 digit <* notFollowedBy anyChar <?> "Parse error"
    nothing  = eof >> return Nothing

-- comment is the last fied and so we exclude the check for a semicolon
pComments :: Parser T.Text
pComments = pText

-- Parses an inner csv field, discarding the separating semicolon
field :: Parser a -> Parser a
field p = p <* semicolon

semicolon :: Parser Char
semicolon = char ';'

-- Parses text fields
-- Text fiemmds cannot contain newline characters
pText :: Parser T.Text
pText = T.pack <$> many (noneOf ";\r\n")

pLine :: Parser T.Text
pLine = T.pack <$> manyTill anyChar endOfLine

discardField :: Parser ()
discardField = void $ field $ manyTill anyChar semicolon
