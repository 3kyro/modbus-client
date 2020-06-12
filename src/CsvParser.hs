{-|
Module : CsvParser
Description: Defines the parser used for csv files


-}
module CsvParser
    ( pCSV
    , ModDatum
    )
where

import           Text.Parsec.Text               ( Parser )
import           Text.Parsec                    ( ParseError
                                                , parse
                                                , char
                                                , many
                                                , noneOf
                                                , digit
                                                , many1
                                                , try
                                                , eof
                                                , optional
                                                , anyChar
                                                , manyTill
                                                , endOfLine
                                                )
import           Control.Applicative            ( (<|>) )
import qualified Data.Text                     as T
import           CsvParser.Spec
import           Data.Word                      ( Word16 )
import           Data.Char                      ( toLower )

-- debbuging parser
run :: Parser a -> String -> Either ParseError a
run p s = parse p "" $ T.pack s

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
        <*  endOfLine

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

pFloat :: Parser Float
pFloat = read <$> d
  where
    d          = (++) <$> integer <*> fractional
    integer    = many1 digit
    fractional = try $ (:) <$> char '.' <*> many digit <|> return []

pWord :: Parser Word16
pWord = field $ read <$> many1 digit

pComments :: Parser T.Text
pComments = field pText

field :: Parser a -> Parser a
field p = p <* optional semicolon

semicolon :: Parser Char
semicolon = char ';'

-- Parses text fields
-- Text fiemmds cannot contain newline characters
pText :: Parser T.Text
pText = T.pack <$> many (noneOf ";\r\n")

pLine :: Parser T.Text
pLine = T.pack <$> manyTill anyChar endOfLine
