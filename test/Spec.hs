import CsvParserSpec (csvParserSpec)
import ReplSpec (replSpec)
import ModDataSpec (modDataSpec)
import ModbusSpec (modbusSpec)
main :: IO ()
main = do
    csvParserSpec
    replSpec
    modDataSpec
    modbusSpec

