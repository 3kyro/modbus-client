-- |
-- Module : OptParser
-- Description: Command line options
--
-- Parses command line options
module OptParser
       ( 
        Opt (..)
       , runOpts 
       )
       where

import Data.IP (IPv4)
import Options.Applicative 
       (
         Parser
       , execParser
       , info
       , (<**>)
       , helper
       , fullDesc
       , progDesc
       , strOption
       , long
       , short
       , metavar
       , value
       , help
       , option
       , auto 
       , switch 
       )   

import Types 
import Data.Word (Word8)

data Opt = Opt
    { inputTemplate :: !FilePath
    , outputFile    :: !FilePath
    , ipAddr        :: !IPv4
    , port          :: !Int
    , floatRepr     :: !ByteOrder
    , repl          :: !Bool
    , uId           :: !Word8
}

-- | Executes the options parser
runOpts :: IO Opt
runOpts = execParser opts
  where
    opts =
        info
        (opt <**> helper)
        (fullDesc <> progDesc "A modbus CLI and web server")

opt :: Parser Opt
opt = Opt 
    <$> parseTemplate 
    <*> parseOutputFile 
    <*> parseIPAddr 
    <*> parsePort 
    <*> parseFloatRepr
    <*> parseRepl
    <*> parseUid

parseTemplate :: Parser String
parseTemplate = strOption
    (  long     "template"
    <> short    't'
    <> metavar  "TEMPLATE"
    <> value    "default_template.csv"
    <> help     "Input template file"
    )

parseOutputFile :: Parser String
parseOutputFile = strOption
    ( long      "output" 
    <> short    'o' 
    <> metavar  "OUTPUT" 
    <> value    "output"
    <> help     "Output file"
    )

parseIPAddr :: Parser IPv4
parseIPAddr = option auto
    (  long     "ip"
    <> short    'i'
    <> metavar  "IPADDRESS"
    <> value    (read "127.0.0.1")
    <> help     "IP address of the modbus master"
    )

parsePort :: Parser Int
parsePort = option auto
    ( long      "port"
    <> short    'p' 
    <> metavar  "PORT" 
    <> value    5502
    <> help     "Port number of the modbus master"
    )

parseFloatRepr :: Parser ByteOrder
parseFloatRepr = option auto
    (  long     "order"
    <> short    'o'
    <> metavar  "BYTE_ORDER"
    <> value    LE
    <> help     "Server byte order"
    )

parseRepl :: Parser Bool
parseRepl = switch 
    ( long      "repl"
    <> short    'r'
    <> help     "Starts an interactive Modbus client"
    )

parseUid :: Parser Word8
parseUid = option auto
    (  long     "id"
    <> short    'd'
    <> metavar  "UNIT_ID"
    <> value    1
    <> help     "Active Modbus unit id"
    )
