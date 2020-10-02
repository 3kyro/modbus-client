import CsvParserSpec ( csvParserSpec ) 
import Test.Hspec ( hspec, Spec )
import ReplSpec (replSpec)
import ModDataSpec (modDataSpec)
main :: IO ()
main = do
    hspec spec
    replSpec
    modDataSpec


spec :: Spec
spec = csvParserSpec

