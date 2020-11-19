module Repl.Help
    ( help
    , helpCompl
    , getHelpCmd
    , helpOutputs
    ) where

import           Control.Monad.Trans (liftIO)

import           Repl.Commands       (commandsCompl, getCommand)
import           Types               (Command (..), Repl)

-- Top level help command
help :: [String] -> Repl ()
help args = liftIO $ putStrLn "" >> mapM_ helpCmd args

getHelpCmd :: String -> String
getHelpCmd str =
    case getCommand str of
        ReadInputRegistersWord    -> hReadInputRegistersWord
        ReadInputRegistersBits    -> hReadInputRegistersBits
        ReadInputRegistersFloat   -> hReadInputRegistersFloat
        ReadHoldingRegistersWord  -> hReadHoldingRegistersWord
        ReadHoldingRegistersBits  -> hReadHoldingRegistersBits
        ReadHoldingRegistersFloat -> hReadHoldingRegistersFloat
        WriteRegistersWord        -> hWriteRegistersWord
        WriteRegistersBits        -> hWriteRegistersBits
        WriteRegistersFloat       -> hWriteRegistersFloat
        Read                      -> hRead
        Write                     -> hWrite
        StartHeartbeat            -> hHeartbeat
        StopHeartbeat             -> hStopHeartbeat
        ListHeartbeat             -> hListHeartbeat
        Import                    -> hImport
        Export                    -> hExport
        Id                        -> hId
        CommandNotFound           -> "Command \"" ++ str ++"\" not found"

helpCmd :: String -> IO ()
helpCmd []  = putStrLn helpMessage
helpCmd arg = putStrLn $ getHelpCmd arg

-- A list of all possible help outputs
-- Used for testing
helpOutputs :: [String]
helpOutputs =
    [ hReadInputRegistersWord
    , hReadInputRegistersBits
    , hReadInputRegistersFloat
    , hReadHoldingRegistersWord
    , hReadHoldingRegistersBits
    , hReadHoldingRegistersFloat
    , hWriteRegistersWord
    , hWriteRegistersBits
    , hWriteRegistersFloat
    , hRead
    , hWrite
    , hHeartbeat
    , hStopHeartbeat
    , hListHeartbeat
    , hImport
    , hExport
    , hId
    ]

helpMessage :: String
helpMessage =
    "For a list of availiable commands, type \":list\"\n"
    ++ "For help on individual commands, type \":help command\" (e.g. :help list)"

hReadInputRegistersWord :: String
hReadInputRegistersWord =
    "readInputRegistersWord:\n"
    ++ "Read values of word type from the provided input registers\n"
    ++ "Usage: readInputRegistersWord [Starting address] [Number of registers]\n"
    ++ "e.g. readInputRegistersWord 3001 10\n"

hReadInputRegistersBits :: String
hReadInputRegistersBits =
    "readInputRegistersBits:\n"
    ++ "Read values of word type from the provided input registers\n"
    ++ "The bits of the values will be printed, starting with the least significant bit\n"
    ++ "Usage: readInputRegistersBits [Starting address] [Number of registers]\n"
    ++ "e.g. readInputRegistersWord 3001 10\n"

hReadInputRegistersFloat :: String
hReadInputRegistersFloat =
    "readInputRegistersFloat:\n"
    ++ "Read values of float type from the provided input registers\n"
    ++ "Usage: readInputRegistersFloat [Starting address] [Number of registers]\n"
    ++ "e.g. readInputRegistersFloat 3001 10\n"

hReadHoldingRegistersWord :: String
hReadHoldingRegistersWord =
    "readHoldingRegistersWord:\n"
    ++ "Read values of word type from the provided holding registers\n"
    ++ "Usage: readHoldingRegistersWord [Starting address] [Number of registers]\n"
    ++ "e.g. readHoldingRegistersWord 3001 10\n"

hReadHoldingRegistersBits :: String
hReadHoldingRegistersBits =
    "readHoldingRegistersBits:\n"
    ++ "Read values of word type from the provided holding registers\n"
    ++ "The bits of the values will be printed, starting with the least significant bit\n"
    ++ "Usage: readHoldingRegistersBits [Starting address] [Number of registers]\n"
    ++ "e.g. readHoldingRegistersBits 3001 10\n"

hReadHoldingRegistersFloat :: String
hReadHoldingRegistersFloat =
    "readHoldingRegistersFloat:\n"
    ++ "Read values of float type from the provided holding registers\n"
    ++ "Usage: readHoldingRegistersFloat [Starting address] [Number of registers]\n"
    ++ "e.g. readHoldingRegistersFloat 3001 10\n"

hWriteRegistersWord :: String
hWriteRegistersWord =
    "writeRegistersWord:\n"
    ++ "Write multiple values of word type, starting from the provided address\n"
    ++ "Usage: writeSingleRegisterWord [starting address] [word values ..]\n"
    ++ "e.g. writeRegistersWord 1001 1 2 3\n"

hWriteRegistersBits :: String
hWriteRegistersBits =
    "writeRegistersBits:\n"
    ++ "Write multiple values of word type, starting from the provided address\n"
    ++ "The values should be provided in raw bit form, starting with the least significant bit\n"
    ++ "All 16 bits must be provided for a valid input\n"
    ++ "Usage: writeSingleRegisterBits [starting address] [word values ..]\n"
    ++ "e.g. writeRegistersWord 10 1000000000000000 0100000000000000 1100000000000000  \n"

hWriteRegistersFloat :: String
hWriteRegistersFloat =
    "writeRegistersFloat:\n"
    ++ "Write multiple values of float type, starting from the provided address\n"
    ++ "Usage: writeSingleRegisterFloat [starting address] [float values ..]\n"
    ++ "e.g. writeRegistersWord 1001 1.1 2.1 3.4\n"

hRead :: String
hRead =
    "read:\n"
    ++ "Read multiple modbus values from the provided csv file\n"
    ++ "The modbus values are identified by the description field\n"
    ++ "Multiple, space separated, descriptions can be read\n"
    ++ "Usage: read [descriptions]\n"
    ++ "e.g. read status power\n"

hWrite :: String
hWrite =
    "write:\n"
    ++ "Write multiple modbus values from the provided csv file\n"
    ++ "The modbus values are identified by the description field\n"
    ++ "Multiple, space separated, descriptions can be written\n"
    ++ "Usage: write [descriptions] [values]\n"
    ++ "e.g. write status 1 power 15.1\n"

hHeartbeat :: String
hHeartbeat =
    "heartbeat:\n"
    ++ "Create a heartbeat signal that increments a counter at a specified interval.\n"
    ++ "Once activated, the selected registers will be written continuously until the signal is stopped.\n"
    ++ "Multiple signals can be spawned together by providing space separated identifier - timer pairs\n"
    ++ "Both a register addrress as well as a ModData description can be used as an identifier\n"
    ++ "Usage: heartbeat [identifier] [timer(ms)]\n"
    ++ "e.g. heartbeat 10 1000 watch_reg 5000"

hStopHeartbeat :: String
hStopHeartbeat =
    "stopHeartbeat:\n"
    ++ "Stop a currently active heartbeat signal.\n"
    ++ "In order to see all active heartbeat signal, you can use the \"listHeartbeat\" command.\n"
    ++ "Multiple signals can be stopped at the same time by providing space separated identifiers\n"
    ++ "Both a register addrress as well as a ModData description can be used as an identifier\n"
    ++ "Usage: stopHeartbeat [identifier]\n"
    ++ "e.g. stopHeartbeat 10 watch_reg"

hListHeartbeat :: String
hListHeartbeat =
    "listHeartbeat:\n"
    ++ "List all currently active heartbeat signals.\n"
    ++ "Usage: listHeartbeat\n"

hImport :: String
hImport =
    "import:\n"
    ++ "Import a Modbus Table from a valid CSV file\n"
    ++ "Warning: importing a new modbus table will overwrite the current table\n"
    ++ "Use export before importing to save any changes\n"
    ++ "Usage: import path-to-csv-file\n"
    ++ "e.g. import ~/path/to/file.csv\n"

hExport :: String
hExport =
    "export:\n"
    ++ "Export the current Modbus Table to a file\n"
    ++ "Usage: export path-to-csv-file\n"
    ++ "e.g. export ~/path/to/file.csv\n"

hId :: String
hId =
    "id:\n"
    ++ "Set the current Modbus unit id\n"
    ++ "Running the command with no arguments will display the active unit id\n"
    ++ "Usage: id unit-id\n"
    ++ "e.g. id 255\n"

helpCompl :: [String]
helpCompl = map (":help " ++) commandsCompl
