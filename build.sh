#!/usr/bin/env stack
-- stack --resolver lts-16.21 script


{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Info as Info
import Prelude hiding (FilePath)

-- Path declarations
-- all paths are relative to the project's root folder

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

mbserverLinuxExec :: FilePath -> FilePath
mbserverLinuxExec basePath = mbServer basePath </> "target" </> "release"

mbserverWinExec :: FilePath -> FilePath
mbserverWinExec basePath = mbServer basePath </> "target" </> "x86_64-pc-windows-msvc" </> "release"

testModbusServer :: FilePath -> FilePath
testModbusServer baseDir = baseDir </> "testModbusServer"

main :: IO ()
main = do
    -- get current directory information
    appRootPath <- pwd
    validDir <- testfile "modbus-serve.cabal"


    echo "-----------------------------------------------------"
    echo "Modbus Serve build tool"
    echo "-----------------------------------------------------"

    -- check if we're at the root fo the project
    if not validDir
    then do
        stderr "Cannot build outside modbus-serve main folder"
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
        echo "building test client"
        echo "-----------------------------------------------------"
        cd $ mbServer appRootPath
        void $ shell "cargo build --release" empty
        echo "copying test client executables"
        let os = Info.os
        case os of
            "linux" -> do
                cp (mbserverLinuxExec appRootPath </> "main") (testModbusServer appRootPath </> "testserver")
                echo "testserver: linux executable copied"
            "windows" -> do
                cp (mbserverWinExec appRootPath </> "main.exe") (testModbusServer appRootPath </> "testserver.exe")
                echo "testserver.exe: windows executable copied"
            _ -> stderr "Cannot deduce machine os. Please perform a manual build :("
