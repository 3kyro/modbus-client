{-|
Module : Main
Description: A Modbus TCP CLI and web server

Modbus-serve is a Modbus TCP command line communication tool and web server.
-}
module Main where

import OptParser (Opt(..), runOpts)

main :: IO ()
main = greet =<< runOpts

greet :: Opt -> IO ()
greet (Opt input output ip port order) = do
    putStrLn    "Opts parsing:"
    putStrLn $  "input: "       <> input
    putStrLn $  "output: "      <> output
    putStrLn $  "ip: "          <> show ip
    putStrLn $  "port: "        <> show port
    putStrLn $  "byte order: "  <> show order
    
