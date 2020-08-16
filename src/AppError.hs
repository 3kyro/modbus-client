module AppError 
    (
      printError
    , printWarning
    , putStrError
    , putStrWarning
    )
    where

import System.Console.ANSI

import Types (AppError (..))

printError :: AppError -> IO ()
printError err = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Error:"
    setSGR [Reset]
    print err

printWarning :: AppError -> IO ()
printWarning err = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Warning:"
    setSGR [Reset]
    print err

putStrError :: String -> IO ()
putStrError wrn = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Error:"
    setSGR [Reset]
    putStrLn wrn

putStrWarning :: String -> IO ()
putStrWarning wrn = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Warning:"
    setSGR [Reset]
    putStrLn wrn
