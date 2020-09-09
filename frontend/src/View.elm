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
    , nav
    , i
    )
import Html.Attributes exposing
    ( class
    , classList
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
    , ConnectStatus (..)
    , ActiveMenu(..)
    )
import View.Connect exposing (viewConnectMenu)
-- import File exposing (..)

view : Model -> Html Msg
view model =
    div [ class "application" ]
        [ viewMenuBar model
        , viewActiveMenu model
        , viewSideBar
        , viewTables model
        , viewCommandBar
        , viewStatusBar model
        ]

viewMenuBar : Model -> Html Msg
viewMenuBar model =
    div
        [ class "menu_bar" ]
        [ label
            [ classList [ ("menu_bar_activated", model.activeMenu == ConnectMenu ) ]
            , onClick <| ChangeActiveMenu ConnectMenu
            ] [ text "Connect" ]
        , label
            [ classList [ ("menu_bar_activated", model.activeMenu == ImportRegistersMenu ) ]
            , onClick <| ChangeActiveMenu ImportRegistersMenu
            ] [ text "Import" ]
        ]

viewActiveMenu : Model -> Html Msg
viewActiveMenu model =
    case model.activeMenu of
        ConnectMenu -> viewConnectMenu model
        ImportRegistersMenu -> viewRegistersLoad model
        NoneActive -> viewEmptyMenu

viewSideBar : Html Msg
viewSideBar =
    nav
        [ class "side_bar" ]
        [ viewSideBarCsv
        , viewSideBarRegisters
        , viewSideBarHeartBeat
        ]

viewSideBarCsv : Html Msg
viewSideBarCsv =
    i
        [ class "material-icons md-24" ]
        [ text "input" ]

viewSideBarRegisters : Html Msg
viewSideBarRegisters =
    i
        [ class "material-icons md-24" ]
        [ text "view_list" ]

viewSideBarHeartBeat : Html Msg
viewSideBarHeartBeat =
    i
        [ class "material-icons md-24" ]
        [ text "favorite_border" ]

viewTables : Model -> Html Msg
viewTables model =
    div [ class "tables" ]
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
            , tbody [] (List.map viewModData model.modData)
            , tfoot [onClick <| RefreshRequest model.modData]
                [ tr []
                    [ th [scope "row", colspan 7 ] [ text "refresh"]
                    ]
                ]
            ]
        ]

viewCommandBar : Html Msg
viewCommandBar =
    div [ class "command_bar" ] []

viewStatusBar : Model -> Html Msg
viewStatusBar model =
    div [ class "status_bar" ] [text <| showStatus model.status]
viewRegistersLoad : Model -> Html Msg
viewRegistersLoad model =
    div [ class "activeMenu" , class "menu_bar_extension" ]
        [ table []
            [ tr []
                [ label [ onClick CsvRequested ] [ text "Load CSV" ]
                , label [] [ showLoadedFileName model ]
                ]
            , tr []
                [ label [ onClick ModDataRequest ] [ text "Load registers"]]
            , tr [] []
            ]
        ]

showLoadedFileName : Model -> Html Msg
showLoadedFileName model =
    case model.csvFileName of
        Nothing -> text ""
        Just name -> text <| "Loaded " ++ name

viewEmptyMenu : Html Msg
viewEmptyMenu = text ""


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