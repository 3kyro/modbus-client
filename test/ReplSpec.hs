module ReplSpec (replSpec) where

import Repl.Commands (
    commandsCompl,
    getCommand,
 )
import Repl.Help (getHelpCmd, helpOutputs)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
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
    , ReadInputRegistersBits
    , ReadInputRegistersFloat
    , ReadHoldingRegistersWord
    , ReadHoldingRegistersBits
    , ReadHoldingRegistersFloat
    , WriteRegistersWord
    , WriteRegistersBits
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