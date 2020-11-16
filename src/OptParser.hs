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

import           Data.IP                    (IPv4)
import           Options.Applicative        (eitherReader, Parser, ReadM, auto, execParser,
                                             flag, fullDesc, help, helper, info,
                                             long, metavar, option,
                                             progDesc, short, strOption, switch,
                                             value, (<**>), (<|>))

import           Data.Word                  (Word32, Word8)
import           Modbus                     (BaudRate (..), WordOrder (..),
                                             ModbusProtocol (..), Parity (..),
                                             StopBits (..))
import qualified System.Hardware.Serialport as SP

data Opt = Opt
    { appMode       :: !AppMode
    , protocol      :: !ModbusProtocol
    , inputTemplate :: !FilePath
    , outputFile    :: !FilePath
    , ipAddr        :: !IPv4
    , port          :: !Int
    , serialPort    :: !String
    , wordOrder     :: !WordOrder
    , uId           :: !Word8
    , timeout       :: !Int
    , baudrate      :: !BaudRate
    , stopbits      :: !StopBits
    , parity        :: !Parity
    , kaOnOff       :: !Bool
    , kaIdle        :: !Word32
    , kaIntv        :: !Word32
    }

data AppMode
    = AppTemplate
    | AppRepl
    | AppWeb
    | NoneSelected

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
    <*> parseProtocol
    <*> parseInput
    <*> parseOutput
    <*> parseIPAddr
    <*> parsePort
    <*> parseSerialPort
    <*> parseFloatRepr
    <*> parseUid
    <*> parseTimeout
    <*> parseBaudRate
    <*> parseStopBits
    <*> parseParity
    <*> parseKaOnOff
    <*> parseKaIdle
    <*> parseKaIntv


parseAppMode :: Parser AppMode
parseAppMode = parseRepl <|> parseServer <|> parseTemplate

parseRepl :: Parser AppMode
parseRepl = flag NoneSelected AppRepl
    ( long      "repl"
    <> short    'r'
    <> help     "Starts an interactive Modbus client"
    )

parseServer :: Parser AppMode
parseServer = flag NoneSelected AppWeb
    ( long      "server"
    <> short    's'
    <> help     "Starts the local web server"
    )

parseTemplate :: Parser AppMode
parseTemplate = flag NoneSelected AppTemplate
    ( long      "template"
    <> short    't'
    <> help     "Parses a template file"
    )

parseProtocol :: Parser ModbusProtocol
parseProtocol = parseTCP <|> parseRTU

parseTCP :: Parser ModbusProtocol
parseTCP = flag ModBusTCP ModBusTCP
    ( long      "TCP"
    <> help     "Starts an TCP Modbus client"
    )

parseRTU :: Parser ModbusProtocol
parseRTU = flag ModBusTCP ModBusRTU
    ( long      "RTU"
    <> help     "Starts an RTU Modbus client"
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

parseSerialPort :: Parser String
parseSerialPort = strOption
    ( long      "serial"
    <> metavar  "SERIALPORT"
    <> value    ""
    <> help     "Serial Port for modbus RTU"
    )

parseFloatRepr :: Parser WordOrder
parseFloatRepr = option auto
    (  long     "order"
    <> short    'o'
    <> metavar  "WORD_ORDER"
    <> value    LE
    <> help     "Server word order"
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
    <> value    10
    <> help     "Timeout in seconds"
    )

parseBaudRate :: Parser BaudRate
parseBaudRate = option baudrateReader
    (  long     "baud"
    <> metavar  "BAUDRATE"
    <> value    (BR SP.CS9600)
    <> help     "Serial port connection speed"
    )

parseStopBits :: Parser StopBits
parseStopBits = option stopbitsReader
    (  long     "stop"
    <> metavar  "STOPBITS"
    <> value    (SB SP.One)
    <> help     "Serial message stop bits"
    )

parseParity :: Parser Parity
parseParity = option parityReader
    (  long     "parity"
    <> metavar  "PARITY"
    <> value    (Parity SP.Odd)
    <> help     "Serial message parity"
    )

parseKaOnOff :: Parser Bool
parseKaOnOff = switch
    (  long     "keepalive"
    <> help     "Enable keep alive"
    )

parseKaIdle :: Parser Word32
parseKaIdle = option auto
    (  long     "idle"
    <> metavar  "KEEPALIVE_IDLE"
    <> value    120
    <> help     "Keep alive idle time in seconds"
    )

parseKaIntv :: Parser Word32
parseKaIntv = option auto
    (  long     "interval"
    <> metavar  "KEEPALIVE_INTERVAL"
    <> value    60
    <> help     "Keep alive interval time in seconds"
    )



---------------------------------------------------------------------------------------------------------------
-- Readers
---------------------------------------------------------------------------------------------------------------

baudrateReader :: ReadM BaudRate
baudrateReader =
    let
        mbr str = case str of
            "110"    -> Right $ BR SP.CS110
            "300"    -> Right $ BR SP.CS300
            "600"    -> Right $ BR SP.CS600
            "1200"   -> Right $ BR SP.CS1200
            "2400"   -> Right $ BR SP.CS2400
            "4800"   -> Right $ BR SP.CS4800
            "9600"   -> Right $ BR SP.CS9600
            "19200"  -> Right $ BR SP.CS19200
            "38400"  -> Right $ BR SP.CS38400
            "57600"  -> Right $ BR SP.CS57600
            "115200" -> Right $ BR SP.CS115200
            _      -> Left $ "\nInvalid baud rate input. \n"
                             ++ "Valid inputs:\n"
                             ++ "110\n300\n600\n"
                             ++ "1200\n2400\n4800\n9600\n"
                             ++ "19200\n38400\n57600\n115200\n"
    in
        eitherReader mbr

stopbitsReader :: ReadM StopBits
stopbitsReader =
    let
        msb str = case str of
            "one" -> Right $ SB SP.One
            "two" -> Right $ SB SP.Two
            _      -> Left $ "\nInvalid stop bits input. \n"
                             ++ "Valid inputs: one, two"
    in
        eitherReader msb

parityReader :: ReadM Parity
parityReader =
    let
        mpr str = case str of
            "odd"  -> Right $ Parity SP.Odd
            "even" -> Right $ Parity SP.Even
            _      -> Left $ "\nInvalid parity input. \n"
                             ++ "Valid inputs: odd, even"
    in
        eitherReader mpr
