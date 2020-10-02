import CsvParserSpec (csvParserSpec)
import ReplSpec (replSpec)
import ModDataSpec (modDataSpec)

main :: IO ()
main = do
    csvParserSpec
    replSpec
    modDataSpec

