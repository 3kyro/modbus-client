import CsvParserSpec (csvParserSpec)
import ReplSpec (replSpec)
import ModDataSpec (modDataSpec)
import ModbusSpec (modbusSpec)
import ServerSpec (serverSpec)
main :: IO ()
main = do
    csvParserSpec
    replSpec
    modDataSpec
    modbusSpec
    serverSpec
