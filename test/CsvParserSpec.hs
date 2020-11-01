module CsvParserSpec where

import Data.Char (
    isDigit,
    toUpper,
 )
import Data.Either (isLeft)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Word (Word16, Word8)
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Property, Testable (property), (==>))
import Text.Parsec (ParseError)

import CsvParser (
    field,
    only,
    overwriteCSVFile,
    pCSV,
    pDesc,
    pFloat,
    pMaybeFloat,
    pMaybeWord,
    pModData,
    pName,
    pRegType,
    pValue,
    pWord,
    parseCSVFile,
    testCSVParser,
 )
import System.Directory (removeFile)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import TestHelper (TestShow (tShow))
import Text.Parsec.Text (Parser)
import Types (
    ModData (ModData),
    ModValue (..),
    NameArb (unNA),
    RegType,
 )

csvParserSpec :: IO ()
csvParserSpec = hspec $ do
    pCSVFile
    pNameSpec
    pRegTypeSpec
    pRegAddrSpec
    pFloatSpec
    pWordSpec
    pValueSpec
    pCommentSpec
    pModDataSpec
    pCSVSpec

pCSVFile :: Spec
pCSVFile =
    describe "Read and write a valid CSV file to disc" $
        it "performs a roundtrip serialization / deserialization" $
            property prop_roundrtip_serialize

pNameSpec :: Spec
pNameSpec = describe "Parse a name field" $ do
    it "parses valid names" $ property prop_valid_name
    it "fails on newlines" $
        testCSVParser (field pName) "description with \n newline;"
            `shouldSatisfy` isLeft

pRegTypeSpec :: Spec
pRegTypeSpec = describe "Parse a register type field" $ do
    it "ignores capitalisation on valid inputs" $ property prop_valid_regType
    it "fails on invalid input" $
        property $ \x ->
            isLeft $ testCSVParser pRegType (x ++ ";")

pRegAddrSpec :: Spec
pRegAddrSpec = describe "Parse a register address field" $ do
    it "parses numeric fields" $
        property $ \x ->
            Right (x :: Word16) == testCSVParser pWord (show x)
    it "fails on invalid input" $
        property $ \x ->
            not (all isDigit x)
                ==> isLeft
                $ testCSVParser (only pWord :: Parser Word16) x

pCommentSpec :: Spec
pCommentSpec = describe "Parse a comment field" $ do
    it "parses text" $ property prop_comment_text
    it "stops parsing on newlines" $
        testCSVParser pDesc "Comment with \n newline"
            `shouldBe` Right (T.pack "Comment with ")
    it "parses unicode comments" $
        testCSVParser pDesc "Ένα σχόλιο"
            `shouldBe` Right (T.pack "Ένα σχόλιο")

pFloatSpec :: Spec
pFloatSpec = describe "Parse a float" $ do
    it "parses floats" $
        property $ \x ->
            Right (Just x) == testCSVParser pMaybeFloat (show x ++ ";")
    it "parses integers as floats" $ property prop_ints_as_floats
    it "parses floats with no fractional (eg 100.)" $ property prop_no_fractional
    it "parses floats with a leading dot (eg .5)" $ property prop_leading_dot
    it "fails on alphabetic input" $ property prop_alphabetic_float

pWordSpec :: Spec
pWordSpec = describe "Parse a word" $ do
    it "parses integers" $
        property $ \x ->
            Right (Just x) == testCSVParser pMaybeWord (show x ++ ";")
    it "fails on floating points" $ property prop_floats_as_words
    it "fails on alphabetic input" $ property prop_alphabetic_word

pValueSpec :: Spec
pValueSpec = describe "Parse a modbus value" $ do
    it "parses words" $
        property $ \x ->
            Right (ModWord (Just x))
                == testCSVParser pValue ("word;" ++ show x ++ ";")
    it "parses floats" $
        property $ \x ->
            Right (ModFloat (Just x))
                == testCSVParser pValue ("float;" ++ show x ++ ";")
    it "ignores upper case words" $
        property $ \x ->
            Right (ModWord (Just x))
                == testCSVParser
                    pValue
                    (capitalizeLetter "word;" (fromIntegral x) ++ show x ++ ";")
    it "ignores upper case floats" $
        property $ \x ->
            Right (ModFloat (Just x))
                == testCSVParser
                    pValue
                    (capitalizeLetter "float;" (round x) ++ show x ++ ";")
    it "return Nothing on words" $
        testCSVParser pValue "word;;"
            == Right
                (ModWord Nothing)
    it "return Nothing on floats" $
        testCSVParser pValue "float;;"
            == Right
                (ModFloat Nothing)
    it "fails on bad data type" $
        property $ \x ->
            isLeft $ testCSVParser pValue $ x ++ ";;"
    it "fails on non numeric inputs - word" $
        property prop_non_numeric_pvalue_word
    it "fails on non numeric inputs - float" $
        property prop_non_numeric_pvalue_float

pModDataSpec :: Spec
pModDataSpec =
    describe "Parse a ModData" $
        it "parses a valid line" $ property prop_valid_datum

pCSVSpec :: Spec
pCSVSpec = describe "Parse a CSV text" $ do
    it "fails on no input" $ testCSVParser pCSV "" `shouldSatisfy` isLeft
    it "parses a valid CSV text" $ property prop_valid_csv
    it "fails on invalid CSV text" $ property prop_invalid_csv

--------------------------------------------------------------------------
-- Property functions
--------------------------------------------------------------------------

prop_roundrtip_serialize :: [ModData] -> Property
prop_roundrtip_serialize mds = monadicIO $ do
    rlt <- run $ do
        let path = "./temp.csv"
        _ <- overwriteCSVFile path mds
        parseResult <- parseCSVFile path
        removeFile path
        case parseResult of
            Left _ -> return False
            Right parsedMds ->
                return $
                    mds == parsedMds
    assert rlt

prop_valid_regType :: RegType -> Int -> Bool
prop_valid_regType rt n =
    let rts = capitalizeLetter (tShow rt) n ++ ";"
     in Right rt == testCSVParser pRegType rts

prop_valid_name :: NameArb -> Bool
prop_valid_name s = Right str == testCSVParser pName (str ++ ";")
  where
    str = unNA s

validName :: String -> Bool
validName str =
    not (null str)
        && head str `elem` validHead
        && all (`elem` validTail) (tail str)
  where
    validHead = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
    validTail = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_"

prop_comment_text :: String -> Property
prop_comment_text s = valid s ==> Right (T.pack s) == testCSVParser pDesc s
  where
    valid = all (`notElem` ";\n\r")

prop_ints_as_floats :: Int -> Bool
prop_ints_as_floats x =
    Right (Just (fromIntegral x)) == testCSVParser pMaybeFloat (show x ++ ";")

prop_no_fractional :: Int -> Bool
prop_no_fractional x = Right (Just x') == testCSVParser pMaybeFloat (show x ++ ".;")
  where
    x' = fromIntegral x

prop_leading_dot :: Int -> Property
prop_leading_dot x =
    x > 0 ==> Right (Just x')
        == testCSVParser
            pMaybeFloat
            ("." ++ show x ++ ";")
  where
    x' = read $ "0." ++ show x

prop_alphabetic_float :: Int -> Int -> Char -> Property
prop_alphabetic_float x y c =
    not (isDigit c)
        && c /= 'e'
        && c `notElem` "-.;"
        && y > 0
        ==> isLeft parseResult
  where
    parseResult = testCSVParser (only pFloat) (integer ++ "." ++ fractional)
    integer = insertChar x c
    fractional = insertChar y c

prop_floats_as_words :: Float -> Bool
prop_floats_as_words x = isLeft $ testCSVParser pMaybeWord (show x ++ ";")

prop_alphabetic_word :: Int -> Char -> Property
prop_alphabetic_word x c =
    not (isDigit c) && c /= ';' ==> isLeft $ testCSVParser pMaybeWord charWord
  where
    charWord = insertChar x c ++ ";"

prop_non_numeric_pvalue_word :: String -> Property
prop_non_numeric_pvalue_word s =
    not (all isDigit s)
        && notElem ';' s
        ==> isLeft
        $ testCSVParser pValue $
            "word;"
                ++ s
                ++ ";"

prop_non_numeric_pvalue_float :: String -> Property
prop_non_numeric_pvalue_float s =
    not (all isDigit s)
        && notElem ';' s
        ==> isLeft
        $ testCSVParser pValue $
            "float;"
                ++ s
                ++ ";"

prop_valid_datum ::
    NameArb ->
    RegType ->
    Word16 ->
    ModValue ->
    Word8 ->
    String ->
    Property
prop_valid_datum nameArb rt reg val uid desc =
    validText desc
        ==> Right mdata
        == testCSVParser pModData (tShow mdata)
  where
    mdata = ModData nm rt reg val uid (T.pack desc)
    validText = all (`notElem` ";\n\r")
    nm = unNA nameArb

prop_valid_csv :: [ModData] -> Bool
prop_valid_csv xs = Right xs == testCSVParser pCSV (firstLineDesc ++ csvs)
  where
    firstLineDesc = "First line description; incuding; multiple; semicolons\n"
    csvs = intercalate "\n" $ tShow <$> xs

prop_invalid_csv :: String -> RegType -> Word16 -> String -> String -> Bool
prop_invalid_csv desc fun reg typ com =
    -- make sure we create an invalid ModData
    let invalidData =
            desc
                ++ ";"
                ++ tShow fun
                ++ ";"
                ++ show reg
                ++ ";"
                ++ typ
                ++ ";;"
                ++ com
                ++ "\n"
     in isLeft $ testCSVParser pCSV ("Description\n" ++ invalidData)

--------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------

-- inserts a character in the textual representation of a number
-- number is inserted in a pseudo random position
-- eg. insertChar 151 'a' = "1a51"
insertChar :: Int -> Char -> String
insertChar x c = intercalate [c] [fst splitted, snd splitted]
  where
    splitted = splitAt modidx x'
    modidx = x `mod` length x'
    x' = show x

-- Capitalizes a single letter of a string
-- the letter is pseudo-randomly selected based on the length
-- of the string
capitalizeLetter :: String -> Int -> String
capitalizeLetter s x = cap $ splitAt modidx s
  where
    cap ([], y) = y
    cap ([x'], y') = toUpper x' : y'
    cap (x', y') = init x' ++ [toUpper $ last x'] ++ y'
    modidx = x `mod` length s

-- Used for checking ModData data types
makeModData ::
    String -> String -> String -> String -> String -> Either ParseError ModData
makeModData desc fun reg val com =
    testCSVParser
        pModData
        (desc ++ ";" <> fun ++ ";" <> reg ++ ";" <> val ++ ";" <> com)
