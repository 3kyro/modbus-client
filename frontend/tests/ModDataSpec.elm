module ModDataSpec exposing (..)

import Expect as Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Http exposing (Expect)
import ModData exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "ModValue"
        [ describe "fromModValueInput"
            [ fuzz fuzzModData "correctly modifies a ModWord" <|
                \md ->
                    case md.modValue of
                        ModWord _ ->
                            Expect.equal
                                (ModWord (Just 2))
                            <|
                                (fromModValueInput md "2").modValue

                        ModBits _ ->
                            Expect.equal (ModBits Nothing) <|
                                (fromModValueInput md "2").modValue

                        ModFloat _ ->
                            Expect.equal
                                (ModFloat (Just (fromFloat 2)))
                            <|
                                (fromModValueInput md "2").modValue
                        ModDouble _ ->
                            Expect.equal
                                (ModDouble (Just (fromFloat 3.14)))
                            <|
                                (fromModValueInput md "3.14").modValue
            , fuzz fuzzModData "correctly modifies a ModBits" <|
                \md ->
                    case md.modValue of
                        ModBits _ ->
                            Expect.equal
                                (ModBits <| bitsFromString "1100110011101010")
                            <|
                                (fromModValueInput md "1100110011101010").modValue

                        ModWord _ ->
                            Expect.equal (ModWord <| Just 1100110011101010) <|
                                (fromModValueInput md "1100110011101010").modValue

                        ModFloat _ ->
                            Expect.equal (ModFloat <| Just (fromFloat 1100110011101010)) <|
                                (fromModValueInput md "1100110011101010").modValue
                        ModDouble _ ->
                            Expect.equal (ModDouble <| Just (fromFloat 1100110011101010)) <|
                                (fromModValueInput md "1100110011101010").modValue
            , fuzz fuzzModData "correctly modifies a ModFloat" <|
                \md ->
                    case md.modValue of
                        ModFloat _ ->
                            Expect.equal
                                (ModFloat <| Just (fromFloat 3.24))
                            <|
                                (fromModValueInput md "3.24").modValue

                        ModWord _ ->
                            Expect.equal (ModWord Nothing) <|
                                (fromModValueInput md "3.24").modValue

                        ModBits _ ->
                            Expect.equal (ModBits Nothing) <|
                                (fromModValueInput md "3.24").modValue

                        ModDouble _ ->
                            Expect.equal (ModDouble Nothing) <|
                                (fromModValueInput md "3.24").modValue
            ]
        ]
