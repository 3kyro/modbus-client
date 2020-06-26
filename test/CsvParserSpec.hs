module CsvParserSpec where

import CsvParser
import Data.Char
  ( isDigit,
    toUpper,
    isAlpha
  )
import Data.Either (isLeft, isRight)
import Data.List (intercalate, intersperse)
import qualified Data.Text as T
import Data.Word (Word16)
import Test.Hspec
import Test.QuickCheck hiding (function)
import Text.Parsec (ParseError)

csvParserSpec :: Spec
csvParserSpec = do
  pDescriptionSpec
  pFunctionSpec
  pFloatSpec
  pWordSpec
  pValueSpec
  pCommentSpec
  pModDataSpec
  pCSVSpec

pDescriptionSpec :: Spec
pDescriptionSpec = describe "Parse a description field" $ do
  it "parses text" $ property prop_description_text
  it "fails on newlines" $
    testCSVParser pDescription "description with \n newline;"
      `shouldSatisfy` isLeft
  it "parses unicode descriptioons" $
    testCSVParser pDescription "Μια περιγραφή;"
      `shouldBe` Right (T.pack "Μια περιγραφή")

pCommentSpec :: Spec
pCommentSpec = describe "Parse a comment field" $ do
  it "parses text" $ property prop_comment_text
  it "stops parsing on newlines" $
    testCSVParser pComments "Comment with \n newline"
      `shouldBe` Right (T.pack "Comment with ")
  it "parses unicode comments" $
    testCSVParser pComments "Ένα σχόλιο"
      `shouldBe` Right (T.pack "Ένα σχόλιο")

pFunctionSpec :: Spec
pFunctionSpec = describe "Parse a function code" $ do
  it "fails on non numeric inputs" $ property prop_non_numeric_function_code
  it "fails at non implemented function codes" $ property prop_non_function_code
  context "it correctly assigns functions codes" $ do
    it "at read input" $
      testCSVParser pFunction "3;"
        `shouldBe` Right ReadInput
    it "at read multiple holding registers" $
      testCSVParser pFunction "4;"
        `shouldBe` Right ReadMultHolding
    it "at write single holding registers" $
      testCSVParser pFunction "6;"
        `shouldBe` Right WriteSingleHolding
    it "at write multiple holding registers" $
      testCSVParser pFunction "16;"
        `shouldBe` Right WriteMultHolding

pFloatSpec :: Spec
pFloatSpec = describe "Parse a float" $ do
  it "parses floats" $ property $ \x ->
    Right (Just x) == testCSVParser pFloat (show x ++ ";")
  it "parses integers as floats" $ property prop_ints_as_floats
  it "parses floats with no fractional (eg 100.)" $
    property prop_no_fractional
  it "parses floats with a leading dot (eg .5)" $ property prop_leading_dot
  it "fails on alphabetic input" $ property prop_alphabetic_float

pWordSpec :: Spec
pWordSpec = describe "Parse a word" $ do
  it "parses integers" $ property $ \x ->
    Right (Just x) == testCSVParser pWord (show x ++ ";")
  it "fails on floating points" $ property prop_floats_as_words
  it "fails on alphabetic input" $ property prop_alphabetic_word

pValueSpec :: Spec
pValueSpec = describe "Parse a modbus value" $ do
  it "parses words" $ property $ \x ->
    Right (ModWord (Just x))
      == testCSVParser pValue ("word;" ++ show x ++ ";")
  it "parses floats" $ property $ \x ->
    Right (ModFloat (Just x))
      == testCSVParser pValue ("float;" ++ show x ++ ";")
  it "ignores upper case words" $ property $ \x ->
    Right (ModWord (Just x))
      == testCSVParser
        pValue
        (capitalizeLetter "word;" (fromIntegral x) ++ show x ++ ";")
  it "ignores upper case floats" $ property $ \x ->
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
  it "fails on bad data type" $ property $ \x ->
    isLeft $ testCSVParser pValue $ x ++ ";;"
  it "fails on non numeric inputs - word" $
    property prop_non_numeric_pvalue_word
  it "fails on non numeric inputs - float" $
    property prop_non_numeric_pvalue_float

pModDataSpec :: Spec
pModDataSpec = describe "Parse a ModData" $ do
  it "parses a valid line" $ property prop_valid_datum
  -- check valid values that will be used in following tests
  it "parses valid values" $ property prop_datum_check_valid_values
  it "fails on wrong description" $ property prop_datum_fail_description
  it "fails on wrong function" $ property prop_datum_fail_function
  it "fails on wrong register" $ property prop_datum_fail_register
  it "fails on wrong value" $ property prop_datum_fail_value
  it "fails on wrong comment" $ property prop_datum_fail_comment

pCSVSpec :: Spec
pCSVSpec = describe "Parse a CSV text" $ do
  it "parses a valid CSV text" $ property prop_valid_csv

--------------------------------------------------------------------------
-- Property functions
--------------------------------------------------------------------------

prop_description_text :: String -> Property
prop_description_text s =
  valid s ==> Right (T.pack s)
    == testCSVParser
      pDescription
      (s ++ ";")
  where
    valid = all (`notElem` ";\n\r")

prop_comment_text :: String -> Property
prop_comment_text s = valid s ==> Right (T.pack s) == testCSVParser pComments s
  where
    valid = all (`notElem` ";\n\r")

prop_non_function_code :: Int -> Property
prop_non_function_code x = x `notElem` [3, 4, 6, 16] ==> isLeft $ testCSVParser pFunction (show x ++ ";")

prop_non_numeric_function_code :: String -> Property
prop_non_numeric_function_code s = not (any isDigit s) && notElem ';' s ==> isLeft $ testCSVParser pFunction (s ++ ";")

prop_ints_as_floats :: Int -> Bool
prop_ints_as_floats x =
  Right (Just (fromIntegral x)) == testCSVParser pFloat (show x ++ ";")

prop_no_fractional :: Int -> Bool
prop_no_fractional x = Right (Just x') == testCSVParser pFloat (show x ++ ".;")
  where
    x' = fromIntegral x

prop_leading_dot :: Int -> Property
prop_leading_dot x =
  x > 0 ==> Right (Just x')
    == testCSVParser
      pFloat
      ("." ++ show x ++ ";")
  where
    x' = read $ "0." ++ show x

prop_alphabetic_float :: Int -> Int -> Char -> Property
prop_alphabetic_float x y c =
  not (isDigit c)
    && c /= 'e'
    && c
    `notElem` "-.;"
    && y
    > 0
    ==> isLeft parseResult
  where
    parseResult = testCSVParser pFloat (integer ++ "." ++ fractional)
    integer = insertChar x c
    fractional = insertChar y c

prop_floats_as_words :: Float -> Bool
prop_floats_as_words x = isLeft $ testCSVParser pWord (show x ++ ";")

prop_alphabetic_word :: Int -> Char -> Property
prop_alphabetic_word x c =
  not (isDigit c) && c /= ';' ==> isLeft $ testCSVParser pWord charWord
  where
    charWord = insertChar x c ++ ";"

prop_non_numeric_pvalue_word :: String -> Property
prop_non_numeric_pvalue_word s =
  not (all isDigit s)
    && notElem ';' s
    ==> isLeft
    $ testCSVParser pValue
    $ "word;"
      ++ s
      ++ ";"

prop_non_numeric_pvalue_float :: String -> Property
prop_non_numeric_pvalue_float s =
  not (all isDigit s)
    && notElem ';' s
    ==> isLeft
    $ testCSVParser pValue
    $ "float;"
      ++ s
      ++ ";"

prop_valid_datum :: String -> ModFunction -> Word16 -> ModType -> String -> Property
prop_valid_datum desc fun reg val com =
  validText desc && validText com
    ==> Right
      ( ModData
          { description = T.pack desc,
            function = fun,
            register = reg,
            value = val,
            comments = T.pack com
          }
      )
      == testCSVParser
        pModData
        ( desc ++ ";"
            <> tShow fun ++ ";"
            <> show reg ++ ";"
            <> tShow val ++ ";"
            <> com
        )
  where
    validText = all (`notElem` ";\n\r")

-- typical valid values, to be used in ModData fail tests
validDesc = "description"

validFun = "3"

validReg = "3000"

validVal = "word;1"

validCom = "comment"

prop_datum_check_valid_values :: Bool
prop_datum_check_valid_values =
  isRight $ makeModData validDesc validFun validReg validVal validCom

prop_datum_fail_description :: Bool

prop_datum_fail_description =
  isLeft $ makeModData desc validFun validReg validVal validCom
  where
    desc = "foo\nbar"

prop_datum_fail_function :: Bool
prop_datum_fail_function =
  isLeft $ makeModData validDesc fun validReg validVal validCom
  where
    fun = "wrong function"

prop_datum_fail_register :: Bool
prop_datum_fail_register =
  isLeft $ makeModData validDesc validFun reg validVal validCom
  where
    reg = "wrong register"

prop_datum_fail_value :: Bool
prop_datum_fail_value =
  isLeft $ makeModData validDesc validFun validReg val validCom
  where
    val = "wrong;value"

prop_datum_fail_comment :: Bool
prop_datum_fail_comment =
  isLeft $ makeModData validDesc validFun validReg validVal com
  where
    com = "wrong ; comment"

prop_valid_csv :: [ModData] -> Bool 
prop_valid_csv xs = Right xs   == testCSVParser pCSV csvs
  where csvs = intercalate "\n" $ tShow <$> xs


--------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------

instance Arbitrary ModFunction where
  arbitrary = elements [ReadInput, ReadMultHolding, WriteSingleHolding, WriteMultHolding]

instance Arbitrary ModType where
  arbitrary = oneof [ModWord <$> arbitrary, ModFloat <$> arbitrary]

instance Arbitrary ModData where
  arbitrary = modData <$> arbText <*> arbitrary <*> arbitrary <*> arbitrary <*> arbText
    where arbText = oneof [return (T.pack ""), T.cons <$> validChar <*> arbText]
          validChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "&éèçà$=:"  

class TestShow a where
  tShow :: a -> String

instance TestShow ModType where
  tShow (ModWord (Just x)) = "word;" ++ show x
  tShow (ModWord Nothing) = "word;"
  tShow (ModFloat (Just x)) = "float;" ++ show x
  tShow (ModFloat Nothing) = "float;" 

instance TestShow ModFunction where
  tShow ReadInput = "3"
  tShow ReadMultHolding = "4"
  tShow WriteSingleHolding = "6"
  tShow WriteMultHolding = "16"

instance TestShow ModData where
  tShow (ModData desc fun reg val com) = 
    T.unpack desc ++ ";" ++
    tShow fun ++ ";" ++
    show reg ++ ";" ++
    tShow val ++ ";" ++
    T.unpack com

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
    cap ([x], y) = toUpper x : y
    cap (x, y) = init x ++ [toUpper $ last x] ++ y
    modidx = x `mod` length s

-- Used for checking ModData data types
makeModData :: String -> String -> String -> String -> String -> Either ParseError ModData
makeModData desc fun reg val com =
  testCSVParser
    pModData
    ( desc ++ ";"
        <> fun ++ ";"
        <> reg ++ ";"
        <> val ++ ";"
        <> com
    )
