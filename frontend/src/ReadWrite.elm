module ReadWrite exposing (..)

import Element
    exposing
        ( Color
        , Element
        , centerY
        , focused
        , padding
        , text
        )
import Element.Background as Background
import Element.Input as Input
import Json.Decode as D
import Json.Encode as E


type ReadWrite
    = Read
    | Write


flipRW : ReadWrite -> ReadWrite
flipRW rw =
    case rw of
        Read ->
            Write

        Write ->
            Read


encodeRW : ReadWrite -> E.Value
encodeRW rw =
    case rw of
        Read ->
            E.string "read"

        Write ->
            E.string "write"


decodeRW : D.Decoder ReadWrite
decodeRW =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "read" ->
                        D.succeed Read

                    "write" ->
                        D.succeed Write

                    _ ->
                        D.fail "Neither Read or Write"
            )


readWriteButton :
    ReadWrite -- Value to render
    -> Color -- Read Color
    -> Color -- Write Color
    -> msg -- Message on change
    -> Element msg
readWriteButton rw clrRead clrWrite msg =
    Input.button
        [ Background.color <| rwButtonBGClr rw clrRead clrWrite
        , centerY
        , padding 3
        , focused []
        ]
        { onPress = Just msg
        , label = readWriteButtonText rw
        }


rwButtonBGClr : ReadWrite -> Color -> Color -> Color
rwButtonBGClr rw clrRead clrWrite =
    case rw of
        Read ->
            clrRead

        Write ->
            clrWrite


readWriteButtonText : ReadWrite -> Element msg
readWriteButtonText rw =
    case rw of
        Read ->
            text "Read"

        Write ->
            text "Write"
