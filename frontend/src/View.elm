module View exposing (view)

import Html exposing
    ( Html
    , div
    , text
    , label
    , table
    , th
    , tr
    , td
    , thead
    , tbody
    , tfoot
    )
import Html.Attributes exposing
    ( class
    , scope
    , colspan
    )
import Html.Events exposing (onClick)

import Types exposing
    ( Msg (..)
    , Model
    , Register (..)
    , getRegType
    , getRegValue
    , ModValue (..)
    , Status (..)
    )

view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "inputRegisters" ] [viewRegisters model.registers]
        , div [] [text <| showStatus model.status]
        ]

showStatus : Status -> String
showStatus status =
    case status of
        AllGood -> "all good"
        Loading -> "getting stuff from the server"
        Bad err -> err

viewRegister : Register -> Html Msg
viewRegister reg =
        tr []
             [ td [] [text <| getRegType reg]
             , td [] [ viewModType <| getRegValue reg ]
             , td [] [ viewModValue <| getRegValue reg ]
             ]


viewRegisters : List Register -> Html Msg
viewRegisters regs =
    div [ class "registers" ]
        [ table [ class "regTable" ]
            [ thead [] <|
                [ tr []
                    [ th [] [ text "Register Type"]
                    , th [] [ text "Value Type" ]
                    , th [] [ text "Value" ]
                    ]
                ]
            , tbody [] (List.map viewRegister regs)
            , tfoot [onClick <| RefreshRequest regs]
                [ tr []
                    [ th [scope "row", colspan 3 ] [ text "refresh"]
                    ]
                ]
            ]
        ]

-- viewModData : ModData -> Html Msg
-- viewModData data =
--     div [ class "modDatum"]
--         [ label [ class "modName" ]
--             [ text data.name ]
--         , label [ class "modRegType" ]
--             [ text <| getRegType data.register ]
--         , label [ class "modAddress" ]
--             [ text <| String.fromInt data.address ]
--         , viewModType <| getRegValue data.register
--         , viewModValue <| getRegValue data.register
--         , label [ class "modDescription" ]
--             [ text data.description ]
--         ]

viewModValue : ModValue -> Html Msg
viewModValue value =
    label [ class "modValue" ]
        <| case value of
            ModWord (Just word) ->
                    [ text <| String.fromInt word]
            ModFloat (Just float) ->
                    [ text <| String.fromFloat float]
            _ -> []
viewModType : ModValue -> Html Msg
viewModType value =
    label [ class "modType" ]
        <| case value of
            ModWord (_) ->
                    [ text "Word" ]
            ModFloat (_) ->
                    [ text "Float" ]