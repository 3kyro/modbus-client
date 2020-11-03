module CsvParser where

import           Control.Monad                     (void)
import           Data.Char                         (toLower)
import           Data.Functor                      (($>))
import           Data.Word                         (Word16)
import           System.Directory                  (doesDirectoryExist,
                                                    doesFileExist)
import           Text.Parsec
import           Text.Parsec.Text                  (Parser)

import qualified Control.Exception                 as E
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T

import           Data.Either.Combinators           (mapLeft)
import           PrettyPrint                       (ppStrError, ppStrWarning)
import           System.Directory.Internal.Prelude (stdout)
import           System.IO                         (hFlush)
import           Types

-- Read and parse a CSV file from the disk
parseCSVFile :: FilePath -> IO (Either AppError [ModData])
parseCSVFile path = do
        ioContents <- E.try $ T.readFile path
        let result = do
                contents <- mapLeft AppIOError ioContents
                runpCSV contents
        return result

-- Serialize ModData on the disk
serializeCSVFile :: FilePath -> [ModData] -> IO (Either AppError ())
serializeCSVFile filename mdata = do
    dir <- doesDirectoryExist filename
    if dir
    then do
        ppStrError $ show filename ++ " is an existing directory\nRegister table not exported"
        return $ Right ()
    else do
        exists <- doesFileExist filename
        if exists
        then do
            ppStrWarning $ "file: " ++ filename ++ " already exists"
            putStr "Type [Y/y] to overwrite: "
            hFlush stdout
            overwrite <- T.toUpper <$> T.getLine
            case T.unpack overwrite of
                "Y" -> overwriteCSVFile filename mdata
                _ -> do
                    putStrLn "Register table not exported"
                    return $ Right ()
        else overwriteCSVFile filename mdata

-- Writes a modbus table to the disk, overwritting the given file
-- if it already exists
overwriteCSVFile :: FilePath -> [ModData] -> IO (Either AppError ())
overwriteCSVFile filename mdata = do
    let text = serializeModData mdata
    ioresult <- E.try $ T.writeFile filename text
    return $ mapLeft AppIOError ioresult

-- test runner, mainly used for testing
testCSVParser :: Parser a -> String -> Either ParseError a
testCSVParser p s = parse p "" $ T.pack s

-- run a pCSV parser
runpCSV :: T.Text -> Either AppError [ModData]
runpCSV t = mapLeft AppParseError (parse pCSV "" t)

-- Parses a CSV text, ignoring the first line that will be used for describing
-- the fields
pCSV :: Parser [ModData]
pCSV = (++) <$> firstLine <*> (many pModData <* eof)
  where
      firstLine = try moddata <|> (pLine $> [])
      moddata = pure <$> pModData

-- Parses a ModData
pModData :: Parser ModData
pModData =
  ModData
    <$> parseWithMsg (field pName) "register name"
    <*> parseWithMsg pRegType "regiter Type"
    <*> parseWithMsg (field pWord) "register address"
    <*> parseWithMsg pValue "register value"
    <*> parseWithMsg (field pWord) "unit id"
    <*> parseWithMsg pDesc "description"
    <* optional endOfLine

-- Apply parser, replacing the error messages with msg when it fails
parseWithMsg :: Parser a -> String -> Parser a
parseWithMsg p msg = try p <?> msg

-- Parses a name cell
pName :: Parser String
pName = (:) <$> oneOf (['A'..'Z'] ++ ['a'..'z'] ++ "_") <*> many (oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")

-- Parses a regiter type
pRegType :: Parser RegType
pRegType = do
  rt <- T.map toLower <$> field pText
  case T.unpack rt of
    -- "discrete input"   -> return DiscreteInput
    -- "coil"             -> return Coil
    "input register"   -> return InputRegister
    "holding register" -> return HoldingRegister
    _                  -> fail ""

-- Parses a register address
pWord :: Read a => Parser a
pWord = read <$> many1 digit

-- Parses a modbus value by associating the datatype field with
-- the correct value field
pValue :: Parser ModValue
pValue = do
  dataType <- T.map toLower <$> field pText
  case T.unpack dataType of
    "float" -> ModFloat <$> pMaybeFloat
    "word"  -> ModWord <$> pMaybeWord
    "bits"  -> ModWordBit <$> pMaybeWordBit
    _       -> fail ""

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
pMaybeWord = Just <$> field pWord <|> nothing
  where
    nothing = char ';' >> return Nothing

-- Parses a WordBit. The parser expects:
-- A textual reresentation of 16 bits, starting with LSB from left to right
pMaybeWordBit :: Parser (Maybe WordBit)
pMaybeWordBit = Just <$> field pBit <|> nothing
  where
    nothing = char ';' >> return Nothing

pBit :: Parser WordBit
pBit = bitsFromBools <$> count 16 (pZero <|> pOne)
  where
    pZero = char '0' $> False
    pOne = char '1' $> True


-- Parses an Int
pInt :: Parser Int
pInt = read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)

-- Parses a float in leading dot format (eg .2)
pFloatLeadingDot :: Parser Float
pFloatLeadingDot = read <$> (char '.' *> addLeadingZero)
  where
    addLeadingZero = (++) "0." <$> many1 digit

-- Parses a float when no fractional is given
pFloatNoFractional :: Parser Float
pFloatNoFractional = read . show <$> pInt <* char '.'

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
pDesc :: Parser T.Text
pDesc = T.pack <$> many (noneOf ";\r\n") <* notFollowedBy (char ';')

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
