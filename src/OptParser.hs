-- |
-- Module : OptParser
-- Description: Command line options
--
-- Parses command line options
module OptParser
       ( 
        Opt (..)
       , AppMode (..)
       , runOpts
       )
       where

import Data.IP (IPv4)
import Options.Applicative 
       (flag, (<|>), 
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

import Types 
import Data.Word (Word8)

data Opt = Opt
    { appMode       :: !AppMode
    , inputTemplate :: !FilePath
    , outputFile    :: !FilePath
    , ipAddr        :: !IPv4
    , port          :: !Int
    , floatRepr     :: !ByteOrder
    , uId           :: !Word8
    , timeout       :: !Int
}

data AppMode
    = AppTemplate
    | AppRepl
    | AppWeb

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
    <$> parseAppMode
    <*> parseInput
    <*> parseOutput
    <*> parseIPAddr
    <*> parsePort
    <*> parseFloatRepr
    <*> parseUid
    <*> parseTimeout

parseAppMode :: Parser AppMode
parseAppMode = parseRepl <|> parseServer

parseRepl :: Parser AppMode
parseRepl = flag AppTemplate AppRepl
    ( long      "repl"
    <> short    'r'
    <> help     "Starts an interactive Modbus client"
    )

parseServer :: Parser AppMode
parseServer = flag AppTemplate AppWeb
    ( long      "server"
    <> short    's'
    <> help     "Starts the local web server"
    )

parseInput :: Parser String
parseInput = strOption
    (  long     "input"
    <> short    'i'
    <> metavar  "INPUT"
    <> value    "input.csv"
    <> help     "Input register table"
    )

parseOutput :: Parser String
parseOutput = strOption
    ( long      "output"
    <> short    'o' 
    <> metavar  "OUTPUT"
    <> value    "output.csv"
    <> help     "Output register table"
    )

parseIPAddr :: Parser IPv4
parseIPAddr = option auto
    (  long     "ip"
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

parseUid :: Parser Word8
parseUid = option auto
    (  long     "id"
    <> short    'd'
    <> metavar  "UNIT_ID"
    <> value    1
    <> help     "Active Modbus unit id"
    )

parseTimeout :: Parser Int
parseTimeout = option auto
    (  long     "timeout"
    <> metavar  "TIMEOUT"
    <> value    1000
    <> help     "Timeout in ms"
    )