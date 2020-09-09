import CsvParserSpec ( csvParserSpec ) 
import Test.Hspec ( hspec, Spec )
import ReplSpec (replSpec)

main :: IO ()
main = do
    hspec spec
    replSpec


spec :: Spec
spec = csvParserSpec

