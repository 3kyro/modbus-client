module PrettyPrint (
    ppError,
    ppWarning,
    ppStrError,
    ppStrWarning,
    ppAndReset,
    ppAndResetLn,
    ppMultModData,
    ppRegisters,
    ppUid,
    ppMultThreadState,
    ppThreadError,
    ppPlaceholderModData,
) where

import Control.Exception (SomeException)
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import System.Console.ANSI

import qualified Data.Text as T

import Modbus (HeartBeat (..))
import Types

ppErrReset :: IO ()
ppErrReset = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "Error:"
    setSGR [Reset]

ppError :: AppError -> IO ()
ppError err = do
    ppErrReset
    print err

ppStrError :: String -> IO ()
ppStrError wrn = do
    ppErrReset
    putStrLn wrn

ppWarnReset :: IO ()
ppWarnReset = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Warning:"
    setSGR [Reset]

ppWarning :: AppError -> IO ()
ppWarning err = do
    ppWarnReset
    print err

ppStrWarning :: String -> IO ()
ppStrWarning wrn = do
    ppWarnReset
    putStrLn wrn

ppModData :: ModData -> IO ()
ppModData md =
    insideMagentaDashes Nothing $ do
        ppAndReset "Name: " Blue
        putStrLn $ modName md
        ppAndReset "Register type: " Blue
        putStr $ show (modRegType md) ++ " "
        ppAndReset "@address: " Blue
        print (modAddress md)
        ppAndReset "Value: " Blue
        print (modValue md)
        ppAndReset "Description: " Blue
        putStrLn $ T.unpack (modDescription md)

ppGetTerminalWidth :: IO Int
ppGetTerminalWidth = do
    size <- getTerminalSize
    let width = snd $ fromMaybe (10, 10) size
    pure width

ppMultModData :: [ModData] -> IO ()
ppMultModData = mapM_ ppModData

ppRegisters :: RegType -> [(Word16, ModValue)] -> IO ()
ppRegisters rt mvs =
    insideMagentaDashes
        ( Just ("Register type", show rt))
        $ mapM_ ppRegister mvs

ppRegister :: (Word16, ModValue) -> IO ()
ppRegister (address, mv) = do
    ppAndReset "@address: " Blue
    putStr $ show address ++ "\t"
    ppAndReset "Value: " Blue
    print mv

ppPlaceholderModData :: [ModData] -> IO ()
ppPlaceholderModData mds =
    insideMagentaDashes
        Nothing
        $ mapM_ ppPlaceholderModDatum mds


ppPlaceholderModDatum :: ModData -> IO ()
ppPlaceholderModDatum md = do
    ppAndReset "@address: " Blue
    putStr $ show (modAddress md) ++ "\t"
    ppAndReset "Value: " Blue
    print $ modValue md

ppUid :: Word8 -> IO ()
ppUid w = do
    ppAndReset "Active unit id: " Blue
    print w

ppAndResetFun :: (String -> IO ()) -> String -> Color -> IO ()
ppAndResetFun f x color = do
    setSGR [SetColor Foreground Vivid color]
    f x
    setSGR [Reset]

ppAndReset :: String -> Color -> IO ()
ppAndReset = ppAndResetFun putStr

ppAndResetLn :: String -> Color -> IO ()
ppAndResetLn = ppAndResetFun putStrLn

ppThreadState :: HeartBeat -> IO ()
ppThreadState state = do
    ppAndReset "@address: " Blue
    putStr $ show (hbAddress state) ++ "\t"
    ppAndReset "Interval: " Blue
    putStrLn $ show (hbInterval state) ++ "ms"

ppMultThreadState :: [HeartBeat] -> IO ()
ppMultThreadState [] = putStrLn "No active heartbeat signals"
ppMultThreadState xs =
    insideMagentaDashes
        Nothing
        $ mapM_ ppThreadState xs

ppThreadError :: HeartBeat -> SomeException -> IO ()
ppThreadError thread exc =
    ppStrError $
        "A heartbeat signal was previously running at address "
            ++ show (hbAddress thread)
            ++ ", but it has stopped after the following exception occured:\n"
            ++ show exc
            ++ "\nThe signal has been removed from the active list"

-- Encloses an action inside magenta dashes that take up the entire width of the terminal
-- An optional header of type (header, value) can be provided. The string will be printed with
-- blue color while the value  will be printed in the default color
insideMagentaDashes :: Maybe (String, String) -> IO () -> IO ()
insideMagentaDashes maybeHeader action = do
    width <- ppGetTerminalWidth
    let dashes = replicate width '-'
    case maybeHeader of
        Nothing -> pure ()
        Just (str, val)-> do
                ppAndReset str Blue
                print val
    ppAndResetLn dashes Magenta
    action
    ppAndResetLn dashes Magenta

