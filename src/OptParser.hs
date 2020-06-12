{-|
Module : OptParser
Description: Command line options

Parses command line options 
-}
module OptParser
    ( Opt(..)
    , runOpts
    )
where

import           Options.Applicative
import           Data.IP

data Opt = Opt {
    inputTemplate :: FilePath,
    outputFile :: FilePath,
    ipAddr :: IPv4,
    port :: Int
}

-- | Executes the options parser
runOpts :: IO Opt
runOpts = execParser opts
  where
    opts = info (opt <**> helper)
                (fullDesc <> progDesc "A modbus CLI and web server")

opt :: Parser Opt
opt = Opt <$> parseTemplate <*> parseOutputFile <*> parseIPAddr <*> parsePort

parseTemplate :: Parser String
parseTemplate = strOption
    (  long "template"
    <> short 't'
    <> metavar "TEMPLATE"
    <> value "default_template.csv"
    <> help "Input template file"
    )

parseOutputFile :: Parser String
parseOutputFile = strOption
    (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file")

parseIPAddr :: Parser IPv4
parseIPAddr = option
    auto
    (  long "ip"
    <> short 'i'
    <> metavar "IPADDRESS"
    <> value (read "192.168.2.1")
    <> help "IP address of the modbus master"
    )

parsePort :: Parser Int
parsePort = option
    auto
    (long "port" <> short 'p' <> metavar "PORT" <> value 502 <> help
        "Port number of the modbus master"
    )
