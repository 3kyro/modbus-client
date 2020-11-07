module TypesSpec exposing (..)

import Expect as Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple, tuple3)
import Http exposing (Expect)
import Test exposing (..)
import Types exposing (deleteListElem, diffList)


heartbeatSpec : Test
heartbeatSpec =
    describe "HeartsBeat"
        [ describe "Manipulation by id"
            [

            ]

        ]
    
utilsSpec : Test
utilsSpec =
    describe "Utils"
        [ describe "diffList"
            [ fuzz (tuple ( list int, list int )) "produces the fifference between two lists" <|
                \( xs, ys ) ->
                    Expect.equal
                        (diffList (ys ++ xs) ys)
                        xs
            ]
        , describe "deleteListElement"
            [ fuzz (tuple3 ( list int, list int, int )) "deletes the first occurence of an item in a list" <|
                \( xs, ys, x ) ->
                    if not (List.member x xs) && not (List.member x ys) then
                        Expect.equal
                            ( deleteListElem
                            x
                            (xs ++ [ x ] ++ ys) )
                            ( xs
                            ++ ys )

                    else if List.member x xs then
                        let
                            filtered =
                                List.filter (\el -> el == x) xs

                            len =
                                List.length xs
                        in
                        Expect.equal
                            (List.length (deleteListElem x xs) )
                                (len - 1)

                    else
                        Expect.false "Expected x not a member" <| List.member x (deleteListElem x xs)
            ]
        ]

