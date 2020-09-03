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
import String

import Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , getRegType
    , ModValue (..)
    , Status (..)
    , showStatus
    , ModData
    , IpAddressByte(..)
    , ConnectStatus (..)
    , ActiveMenu(..)
    )
import View.Connect exposing (viewConnectMenu)

view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ viewMenu model
        , div [ class "inputRegisters" ] [viewMultModData model.modData]
        , div [ class "status" ] [text <| showStatus model.status]
        ]

viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "menu" ]
        [ viewMenuBar model
        , viewActiveMenu model
        ]

viewMenuBar : Model -> Html Msg
viewMenuBar model =
    div
        [ class "menuBar" ]
        [ label [ class "menuButton", onClick <| ChangeActiveMenu ConnectMenu ] [ text "Connect" ]
        , label [ class "menuButton", onClick <| ChangeActiveMenu ImportRegisters ] [ text "Import" ]
        ]

viewActiveMenu : Model -> Html Msg
viewActiveMenu model =
    case model.activeMenu of
        ConnectMenu -> viewConnectMenu model
        ImportRegisters -> viewImportRegistersMenu
        NoneActive -> viewEmptyMenu

viewImportRegistersMenu : Html Msg
viewImportRegistersMenu = div [ class "activeMenu" ] [text "put something here"]

viewEmptyMenu : Html Msg
viewEmptyMenu = div [] []

viewMultModData : List ModData -> Html Msg
viewMultModData mds =
    div [ class "registers" ]
        [ table [ class "regTable" ]
            [ thead [] <|
                [ tr []
                    [ th [] [ text "Name"]
                    , th [] [ text "Type" ]
                    , th [] [ text "Address" ]
                    , th [] [ text "Value Type" ]
                    , th [] [ text "Value" ]
                    , th [] [ text "Unit Id" ]
                    , th [] [ text "Description" ]
                    ]
                ]
            , tbody [] (List.map viewModData mds)
            , tfoot [onClick <| RefreshRequest mds]
                [ tr []
                    [ th [scope "row", colspan 7 ] [ text "refresh"]
                    ]
                ]
            ]
        ]

viewModData : ModData -> Html Msg
viewModData md =
    tr []
        [ td [] [ text md.modName ]
        , td [] [ text <| getRegType md.modRegType ]
        , td [] [ text <| String.fromInt md.modAddress ]
        , td [] [ viewModType md.modValue ]
        , td [] [ viewModValue md.modValue ]
        , td [] [ text <| String.fromInt md.modUid ]
        , td [] [ text md.modDescription ]
        ]

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