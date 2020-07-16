import CsvParserSpec 
import Test.Hspec
import ModbusSpec

main :: IO ()
main = do
    hspec spec
    modbusSpec

spec :: Spec
spec = csvParserSpec

