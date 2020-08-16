module PrettyPrint
    (
      ppError
    , ppWarning
    , ppStrError
    , ppStrWarning
    , ppAndReset
    , ppAndResetLn
    , ppSingleModData
    , ppMultModData
    )
    where

import System.Console.ANSI
import Data.Maybe (fromMaybe)

import Types (ModData(..), AppError (..))
import qualified Data.Text as T

ppError :: AppError -> IO ()
ppError err = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Error:"
    setSGR [Reset]
    print err

ppWarning :: AppError -> IO ()
ppWarning err = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Warning:"
    setSGR [Reset]
    print err

ppStrError :: String -> IO ()
ppStrError wrn = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Error:"
    setSGR [Reset]
    putStrLn wrn

ppStrWarning :: String -> IO ()
ppStrWarning wrn = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Warning:"
    setSGR [Reset]
    putStrLn wrn

ppModData :: Int -> ModData ->  IO ()
ppModData width md = do
    let dashes = take (width) (repeat '-')
    ppAndResetLn dashes Magenta
    ppAndReset "Name: " Blue
    putStrLn $ modName md
    ppAndReset "Register type: " Blue
    putStr $ show (modRegType md) ++ " "
    ppAndReset "@address: " Blue
    putStrLn $ show (modAddress md)
    ppAndReset "Value: " Blue
    putStrLn $ show (modValue md)
    ppAndReset "Description: " Blue
    putStrLn $ T.unpack (modDescription md)
    ppAndResetLn dashes Magenta

ppGetTerminalWidth :: IO Int
ppGetTerminalWidth = do
    size <- getTerminalSize
    let width = snd $ fromMaybe (10,10) size
    pure width

ppSingleModData :: ModData -> IO ()
ppSingleModData md = do
    width <- ppGetTerminalWidth
    ppModData width md

-- So that we don't calculate terminal width for each ModData
ppMultModData :: [ModData] -> IO ()
ppMultModData mds = do
    width <- ppGetTerminalWidth
    mapM_ (ppModData width) mds

ppAndResetFun :: (String -> IO ()) -> String -> Color -> IO ()
ppAndResetFun f x color = do
    setSGR [SetColor Foreground Vivid color]
    f x
    setSGR [Reset]

ppAndReset :: String -> Color -> IO ()
ppAndReset = ppAndResetFun putStr

ppAndResetLn :: String -> Color -> IO ()
ppAndResetLn = ppAndResetFun putStrLn
