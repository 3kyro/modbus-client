module ReplSpec (replSpec)
    where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import Repl.Commands
    ( commandsCompl
      , getCommand )
import Repl.Help (getHelpCmd, helpOutputs)

import Types (Command (..))

replSpec :: IO ()
replSpec = hspec $ do
    autocompleteSpec
    helpSpec

autocompleteSpec :: Spec
autocompleteSpec =
    describe "All commands are correctly autocompleted" $
        it "checks that all commands are valid" $
            map getCommand commandsCompl
            `shouldBe` commands

commands :: [Command]
commands =
    [ ReadInputRegistersWord
    , ReadInputRegistersFloat
    , ReadHoldingRegistersWord
    , ReadHoldingRegistersFloat
    , WriteRegistersWord
    , WriteRegistersFloat
    , Read
    , Write
    , Heartbeat
    , StopHeartbeat
    , ListHeartbeat
    , Import
    , Export
    , Id
    ]

helpSpec :: Spec
helpSpec =
    describe "All commands have help output" $
        it "checks all commands" $
             map getHelpCmd commandsCompl `shouldBe` helpOutputs