module CsvParserSpec where

import           Test.Hspec
import           Test.QuickCheck
import           CsvParser
import qualified Data.Text                     as T
import           Data.Either                    ( isLeft )
import           Data.List                      ( intercalate )
import           Data.Char                      ( isDigit )

csvParserSpec :: Spec
csvParserSpec = do
    pDescriptionSpec
    pFunctionSpec
    pFloatSpec

pDescriptionSpec :: Spec
pDescriptionSpec = describe "Parse a description field" $ do
    it "parses non space descriptions"
        $          testCSVParser pDescription "description;"
        `shouldBe` Right (T.pack "description")
    it "parses descriptions with spaces"
        $          testCSVParser pDescription "description with space;"
        `shouldBe` Right (T.pack "description with space")
    it "fails on newlines"
        $ testCSVParser pDescription "description with \n newline;"
        `shouldSatisfy` isLeft

pFunctionSpec :: Spec
pFunctionSpec = describe "Parse a function code" $ do
    it "fails on non numeric inputs"
        $               testCSVParser pFunction "3non_°numeric"
        `shouldSatisfy` isLeft
    context "it correctly assigns functions codes" $ do
        it "at read input"
            $          testCSVParser pFunction "3;"
            `shouldBe` Right ReadInput
        it "at read multiple holding registers"
            $          testCSVParser pFunction "4;"
            `shouldBe` Right ReadMultHolding
        it "at write single holding registers"
            $          testCSVParser pFunction "6;"
            `shouldBe` Right WriteSingleHolding
        it "at write multiple holding registers"
            $          testCSVParser pFunction "16;"
            `shouldBe` Right WriteMultHolding
    context "it fails at non implemented function codes" $ mapM_
        testFunctionFailure
        [1, 2, 5, 15, 23, 22, 24, 20, 21, 07, 08, 11, 12, 17, 43]

pFloatSpec :: Spec
pFloatSpec = describe "Parse a float" $ do
    it "parses floats" $ property $ \x ->
        Right (Just x) == testCSVParser pFloat (show x)
    it "parses integers as floats" $ property prop_ints_as_floats
    it "parses floats with no fractional (eg 100.)"
        $ property prop_no_fractional
    it "parses floats with a leading dot (eg .5)" $ property prop_leading_dot
    it "fails on alphabetic input" $ property prop_alphabetic_float

testFunctionFailure :: Int -> SpecWith (Arg Expectation)
testFunctionFailure x =
    it ("fails for function code " <> s)
        $               testCSVParser pFunction (s <> ";")
        `shouldSatisfy` isLeft
    where s = show x

prop_ints_as_floats :: Int -> Bool
prop_ints_as_floats x =
    Right (Just (fromIntegral x)) == testCSVParser pFloat (show x)

prop_no_fractional :: Int -> Bool
prop_no_fractional x = Right (Just x') == testCSVParser pFloat (show x <> ".")
    where x' = fromIntegral x

prop_leading_dot :: Int -> Property
prop_leading_dot x = x > 0 ==> Right (Just x') == testCSVParser
    pFloat
    ("." <> show x)
    where x' = read $ "0." <> show x

prop_alphabetic_float :: Int -> Int -> Char -> Property
prop_alphabetic_float x y c =
    not (isDigit c)
        &&  c
        /=  'e'
        &&  y
        >   0
        ==> Right Nothing
        ==  parseResult
        ||  isLeft parseResult
  where
    parseResult = testCSVParser pFloat (integer ++ "." ++ fractional)
    integer     = insertChar x c
    fractional  = insertChar y c

-- inserts a character in the textual representation of a number
-- number is inserted in a pseudo random position
-- eg. insertChar 151 'a' = "1a51"
insertChar :: Int -> Char -> String
insertChar x c = intercalate [c] [fst splitted, snd splitted]
  where
    splitted = splitAt modidx x'
    modidx   = x `mod` length x'
    x' = show x

