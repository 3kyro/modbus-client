#!/usr/bin/env stack
-- stack --resolver lts-16.21 script


{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Info as Info
import Prelude hiding (FilePath)

-- Path declarations
-- all paths are relative to the project's root folder

data OS 
    = Linux
    | Windows
    | Other
    
getOs :: OS
getOs = 
    case Info.os of
        "linux" -> Linux
        "windows" -> Windows
        "mingw32" -> Windows
        _ -> Other

frontend :: FilePath -> FilePath
frontend basePath = basePath </> "frontend"

buildFrontend :: FilePath -> FilePath
buildFrontend basePath = build basePath </> "frontend"

elmInput :: FilePath -> FilePath
elmInput basePath = frontend basePath </> "src" </> "App.elm"

elmOutput :: FilePath -> FilePath
elmOutput basePath = buildFrontend basePath  </> "app.js"

build :: FilePath -> FilePath
build basePath = basePath </> "build"

mbServer :: FilePath -> FilePath
mbServer basePath = testModbusServer basePath </> "mbserver"

mbserverExec :: FilePath -> FilePath
mbserverExec basePath = mbServer basePath </> "target" </> "release"



testModbusServer :: FilePath -> FilePath
testModbusServer baseDir = baseDir </> "testModbusServer"

main :: IO ()
main = do
    -- get current directory information
    appRootPath <- pwd
    validDir <- testfile "modbus-client.cabal"


    echo "-----------------------------------------------------"
    echo "Modbus Serve build tool"
    echo "-----------------------------------------------------"

    -- check if we're at the root fo the project
    if not validDir
    then do
        stderr "Cannot build outside modbus-client main folder"
        exit $ ExitFailure 1
    else do
        -- parse all relevant files
        echo "-----------------------------------------------------"
        echo "building haskell backend using stack"
        echo "-----------------------------------------------------"
        case toText $ build appRootPath of
            Left _ -> do
                stderr "Error getting build directory path"
                exit $ ExitFailure 1
            Right buildDir ->
                void $ proc "stack" ["build", "--copy-bins", "--local-bin-path", buildDir] empty

        echo "-----------------------------------------------------"
        echo "building elm frontend"
        echo "-----------------------------------------------------"
        cd $ frontend appRootPath
        case (,) <$> toText (elmInput appRootPath)
                 <*> toText (elmOutput appRootPath) of
            Left _ -> do
                stderr "Error getting frontend directory path"
                exit $ ExitFailure 1
            Right (inputPath, outputPath) -> do
                void $ proc "elm" ["make", inputPath, "--output", outputPath] empty
        echo "-----------------------------------------------------"
        echo "copying frontend files"
        echo "-----------------------------------------------------"
        echo "copying index.html"
        cp (frontend appRootPath </> "index.html")  (buildFrontend appRootPath </> "index.html")
        echo "copying finished"
        cd appRootPath
        
        echo "-----------------------------------------------------"
        echo "copying files"
        echo "-----------------------------------------------------"
        cp (appRootPath </> "LICENSE")  (build appRootPath </> "LICENSE")
        cp (appRootPath </> "sample.csv")  (build appRootPath </> "sample.csv")

        
        case getOs of
            Windows -> do
                echo "-----------------------------------------------------"
                echo "copying windows installer"
                echo "-----------------------------------------------------"
                cp (appRootPath </> "install.nsi")  (build appRootPath </> "install.nsi")

                echo "-----------------------------------------------------"
                echo "building windows installer"
                echo "-----------------------------------------------------"
                cd $ build appRootPath
                void $ proc "makensis" ["install.nsi"] empty
            _ -> return ()

        echo "-----------------------------------------------------"
        echo "building test client"
        echo "-----------------------------------------------------"
        cd $ mbServer appRootPath
        void $ shell "cargo build --release" empty
        echo "copying test client executables"
        case getOs of
            Linux -> do
                cp (mbserverExec appRootPath </> "main") (testModbusServer appRootPath </> "testserver")
                echo "testserver: linux executable copied"
            Windows -> do
                cp (mbserverExec appRootPath </> "main.exe") (testModbusServer appRootPath </> "testserver.exe")
                echo "testserver.exe: windows executable copied"
            Other -> stderr "Cannot deduce machine os. Please build the test server manually :("
