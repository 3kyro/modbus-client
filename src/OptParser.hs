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
       )   

import CsvParser (ByteOrder (..))

data Opt = Opt
    { inputTemplate :: !FilePath
    , outputFile    :: !FilePath
    , ipAddr        :: !IPv4
    , port          :: !Int
    , floatRepr     :: !ByteOrder
    , repl          :: !Bool
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

parseTemplate :: Parser String
parseTemplate = strOption
    (  long     "template"
    <> short    't'
    <> metavar  "TEMPLATE"
    <> value    "default_template.csv"
    <> help     "Input template file"
    )

parseOutputFile :: Parser String
parseOutputFile =
    strOption
        (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")

parseIPAddr :: Parser IPv4
parseIPAddr = option auto
    (  long     "ip"
    <> short    'i'
    <> metavar  "IPADDRESS"
    <> value    (read "192.168.2.1")
    <> help     "IP address of the modbus master"
    )

parsePort :: Parser Int
parsePort = option auto
    ( long      "port" 
    <> short    'p' 
    <> metavar  "PORT" 
    <> value    502
    <> help     "Port number of the modbus master"
    )

parseFloatRepr :: Parser ByteOrder
parseFloatRepr = option auto
    (  long     "order"
    <> short    'o'
    <> metavar  "BYTE_ORDER"
    <> value    LE
    <> help     "Data items byte order"
    )

parseRepl :: Parser Bool
parseRepl = option auto 
    ( long      "repl"
    <> short    'r'
    <> metavar  "REPL"
    <> value    False
    <> help     "Starts an interactive Modbus client"
    )