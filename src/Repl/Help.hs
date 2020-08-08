module Repl.Help (help) where

import Control.Monad.Trans (liftIO)

import Repl.Types (Repl)

-- Top level help command
help :: [String] -> Repl ()
help args = liftIO $ putStrLn "" >> mapM_ helpCmd args

helpCmd :: String -> IO ()
helpCmd [] = putStrLn helpMessage
helpCmd arg =
  let 
    command = case arg of    
        "readInputRegistersWord" -> hReadInputRegistersWord
        "readInputRegistersFloat" -> hReadInputRegistersFloat
        "readHoldingRegistersWord" -> hReadHoldingRegistersWord
        "readHoldingRegistersFloat" -> hReadHoldingRegistersFloat
        "writeSingleRegisterWord" -> hWriteSingleRegisterWord
        "writeSingleRegisterFloat" -> hWriteSingleRegisterFloat
        "writeMultipleRegistersWord" -> hWriteMultipleRegistersWord
        "writeMultipleRegistersFloat" -> hWriteMultipleRegistersFloat
        "read" -> hRead
        "write" -> hWrite
        _ -> "Command \"" ++ arg ++"\" not found" 
  in
    putStrLn command

helpMessage :: String
helpMessage = 
    "For a list of availiable commands, type \":list\"\n"
    ++ "For help on individual commands, type \":help command\" (e.g. :help list)"

hReadInputRegistersWord :: String
hReadInputRegistersWord = 
    "readInputRegistersWord:\n"
    ++ "Read values of word type from the provided input registers\n"
    ++ "Usage: readInputRegistersWord [Starting address] [Number of registers]\n"

hReadInputRegistersFloat :: String
hReadInputRegistersFloat =
    "readInputRegistersFloat:\n"
    ++ "Read values of float type from the provided input registers\n"
    ++ "Usage: readInputRegistersFloat [Starting address] [Number of registers]\n"

hReadHoldingRegistersWord :: String
hReadHoldingRegistersWord = 
    "readHoldingRegistersWord:\n"
    ++ "Read values of word type from the provided holding registers\n"
    ++ "Usage: readHoldingRegistersWord [Starting address] [Number of registers]\n"

hReadHoldingRegistersFloat :: String
hReadHoldingRegistersFloat =
    "readHoldingRegistersFloat:\n"
    ++ "Read values of float type from the provided holding registers\n"
    ++ "Usage: readHoldingRegistersFloat [Starting address] [Number of registers]\n"
    
hWriteSingleRegisterWord :: String
hWriteSingleRegisterWord =
    "writeSingleRegisterWord:\n"
    ++ "Write a single values of word type to the provided register\n"
    ++ "Usage: writeSingleRegisterWord [register address] [word value]\n"

hWriteMultipleRegistersWord :: String
hWriteMultipleRegistersWord =
    "writeMultipleRegistersWord:\n"
    ++ "Write multiple values of word type, starting from the provided address\n"
    ++ "Usage: writeSingleRegisterWord [starting address] [word values ..]\n"
    
hWriteSingleRegisterFloat :: String
hWriteSingleRegisterFloat =
    "writeSingleRegisterFloat:\n"
    ++ "Write a single values of float type to the provided register\n"
    ++ "Usage: writeSingleRegisterFloat [register address] [float value]\n"

hWriteMultipleRegistersFloat :: String
hWriteMultipleRegistersFloat =
    "writeMultipleRegistersFloat:\n"
    ++ "Write multiple values of float type, starting from the provided address\n"
    ++ "Usage: writeSingleRegisterFloat [starting address] [float values ..]\n"

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