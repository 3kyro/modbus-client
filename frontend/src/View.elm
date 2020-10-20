module View exposing (view)

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , IndexedColumn
        , alignLeft
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , focused
        , height
        , htmlAttribute
        , indexedTable
        , layout
        , mouseOver
        , none
        , padding
        , paddingXY
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Notifications
    exposing
        ( StatusBarState(..)
        , expandButton
        , renderNotifications
        )
import Palette
    exposing
        ( blueSapphire
        , darkGrey
        , fireBrick
        , grey
        , greyWhite
        , lightGreen
        , lightGrey
        , smallFont
        , white
        )
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , ModData
        , ModDataUpdate
        , Model
        , Msg(..)
        , ReadWrite(..)
        , flipRW
        , getModValue
        , getModValueType
        , getRegType
        , showConnectStatus
        , writeableReg
        , DummyOption (..)
        )
import Types.IpAddress
    exposing
        ( IpAddress
        , IpAddressByte(..)
        , showIpAddressByte
        )
import Settings exposing
    ( renderSettings
    )
import Settings exposing (renderSettings)

import Dropdown exposing (..)

view : Model -> Html Msg
view model =
    layout [] <| page model


page : Model -> Element Msg
page model =
    column
        [ Background.color grey
        , width fill
        , height fill
        , smallFont
        ]
        [ menuBar model
        , mainTab model
        , notifications model
        ]



------------------------------------------------------------------------------------------------------------------
-- Menu Bar


menuBar : Model -> Element Msg
menuBar model =
    row
        [ Background.color darkGrey
        , alignTop
        , width fill
        , height <| px 38
        , spacing 0
        ]
        [ connectTabButton model
        , importRegisterTabButton model
        , inputRegisterTabButton model
        , holdingRegisterTabButton model
        , registerTabButton model
        , heartbeatTabButton model
        , settingsTabButton model
        ]



-- Creates a new menu tab selection button


newSelectTabButton : Model -> String -> ActiveTab -> Element Msg
newSelectTabButton model str tab =
    Input.button
        [ Background.color <| selectTabButtonBgdColor model tab
        , focused [ Font.color white ]
        , mouseOver [ Font.color white ]
        , Border.width 0
        , height fill
        , paddingXY 10 0
        , Font.color greyWhite
        ]
        { onPress = Just <| ChangeActiveTab tab
        , label = text str
        }


selectTabButtonBgdColor : Model -> ActiveTab -> Color
selectTabButtonBgdColor model tab =
    if model.activeTab == tab then
        grey

    else
        darkGrey


connectTabButton : Model -> Element Msg
connectTabButton model =
    newSelectTabButton model "Connect" ConnectMenu


importRegisterTabButton : Model -> Element Msg
importRegisterTabButton model =
    newSelectTabButton model "Import" ImportMenu


inputRegisterTabButton : Model -> Element Msg
inputRegisterTabButton model =
    newSelectTabButton model "Input Registers" InputRegistersTab


holdingRegisterTabButton : Model -> Element Msg
holdingRegisterTabButton model =
    newSelectTabButton model "Holding Registers" HoldingRegistersTab


registerTabButton : Model -> Element Msg
registerTabButton model =
    newSelectTabButton model "Register Table" ModDataTab


heartbeatTabButton : Model -> Element Msg
heartbeatTabButton model =
    newSelectTabButton model "Heartbeat Signals" HeartbeatTab

settingsTabButton : Model -> Element Msg
settingsTabButton model =
    newSelectTabButton model "Settings" SettingsTab


mainTab : Model -> Element Msg
mainTab model =
    row
        [ width fill
        , height fill
        ]
        [ infoArea model
        , commandArea model
        ]


infoArea : Model -> Element Msg
infoArea model =
    case model.activeTab of
        ConnectMenu ->
            connectTab model

        ImportMenu ->
            importTab model

        InputRegistersTab ->
            inputRegistersTab

        HoldingRegistersTab ->
            holdingRegistersTab

        ModDataTab ->
            modDataTab model

        HeartbeatTab ->
            heartbeatTab model

        SettingsTab ->
            settingsTab model



------------------------------------------------------------------------------------------------------------------
-- Connect Menu


connectTab : Model -> Element Msg
connectTab model =
    el
        [ Background.color grey
        , width fill
        , height fill
        ]
    <|
        connectIsland model


connectIsland : Model -> Element Msg
connectIsland model =
    column
        [ Background.color grey
        , centerX
        , centerY
        , width <| px 500
        , height <| px 500
        , spacing 20
        , paddingXY 10 20
        ]
        [ ipaddress model
        , portNum model
        , timeout model
        , connectButton model
        , disconnectButton model
        ]


ipaddress : Model -> Element Msg
ipaddress model =
    row
        [ spacing 5
        , Font.color white
        ]
        [ el [ width <| px 100 ] <| text "IP Address"
        , ipAddressInput Byte0 model.ipAddress
        , ipAddressInput Byte1 model.ipAddress
        , ipAddressInput Byte2 model.ipAddress
        , ipAddressInput Byte3 model.ipAddress
        ]


ipAddressInput : IpAddressByte -> IpAddress -> Element Msg
ipAddressInput byte ip =
    Input.text
        [ width <| px 70
        , Background.color lightGrey
        , htmlAttribute <| Html.Attributes.maxlength 3
        , Font.color white
        , focused [ Border.glow white 1 ]
        ]
        { onChange = ChangeIpAddress byte
        , text = showIpAddressByte byte ip
        , placeholder = Nothing
        , label = Input.labelHidden "Byte"
        }


portNum : Model -> Element Msg
portNum model =
    Input.text
        [ width <| px 70
        , Background.color lightGrey
        , htmlAttribute <| Html.Attributes.maxlength 5
        , Font.color white
        , focused [ Border.glow white 1 ]
        ]
        { onChange = ChangePort
        , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.socketPort
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.color white ] <| el [ width <| px 100 ] (text "Port")
        }


timeout : Model -> Element Msg
timeout model =
    Input.text
        [ width <| px 70
        , Background.color lightGrey
        , htmlAttribute <| Html.Attributes.maxlength 5
        , Font.color white
        , focused [ Border.glow white 1 ]
        ]
        { onChange = ChangeTimeout
        , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.timeout
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.color white ] <| el [ width <| px 100 ] (text "Timeout in seconds")
        }


connectButton : Model -> Element Msg
connectButton model =
    Input.button
        [ Background.color <| connectButtonBgd model
        , mouseOver [ Font.color white ]
        , height <| px 38
        , width fill
        , paddingXY 0 0
        , Font.color greyWhite
        , Font.center
        , focused []
        ]
        { onPress =
            case model.connectStatus of
                 Connect -> Just ConnectRequest
                 _ -> Nothing
        , label = text <| showConnectStatus model.connectStatus
        }


connectButtonBgd : Model -> Color
connectButtonBgd model =
    case model.connectStatus of
        Connect ->
            lightGrey

        Connecting ->
            lightGrey

        Connected ->
            lightGreen

        Disconnecting ->
            lightGrey




disconnectButton : Model -> Element Msg
disconnectButton model =
    Input.button
        [ Background.color <| disconnectButtonBgd model
        , mouseOver [ Font.color white ]
        , height <| px 38
        , width fill
        , paddingXY 0 0
        , Font.color greyWhite
        , Font.center
        , focused []
        ]
        { onPress = Just DisconnectRequest
        , label = text "Disconnect"
        }


disconnectButtonBgd : Model -> Color
disconnectButtonBgd model =
    case model.connectStatus of
        Connecting ->
            grey

        Connected ->
            lightGrey

        Disconnecting ->
            grey

        Connect ->
            grey



------------------------------------------------------------------------------------------------------------------
-- Import Menu


importTab : Model -> Element Msg
importTab model =
    el
        [ Background.color grey
        , width fill
        , height fill
        ]
    <|
        importActiveTab model


importActiveTab : Model -> Element Msg
importActiveTab model =
    column
        [ Background.color grey
        , width <| px 500
        , height <| px 500
        , spacing 20
        , paddingXY 10 20
        , centerX
        , centerY
        ]
        [ loadCSVButton
        , loadRegisterTableButton model
        ]


loadCSVButton : Element Msg
loadCSVButton =
    Input.button
        [ Background.color lightGrey
        , mouseOver [ Font.color white ]
        , height <| px 38
        , width fill
        , paddingXY 0 0
        , Font.color greyWhite
        , Font.center
        , focused []
        ]
        { onPress = Just CsvRequested
        , label = text "Load CSV File"
        }


showCSVFile : Model -> Element Msg
showCSVFile model =
    case model.csvFileName of
        Nothing ->
            none

        Just filename ->
            el
                [ Font.color white, centerX ]
                (text <| csvLoadButtonText model.csvLoaded filename)


csvLoadButtonText : Bool -> String -> String
csvLoadButtonText flag str =
    if flag then
        str ++ " registers imported!"

    else
        "Import registers from " ++ str


loadRegisterTableButton : Model -> Element Msg
loadRegisterTableButton model =
    Input.button
        [ Background.color <| loadRegsButtonClr model
        , mouseOver [ Font.color white ]
        , height <| px 38
        , width fill
        , paddingXY 0 0
        , Font.color greyWhite
        , Font.center
        , focused []
        ]
        { onPress = Just ModDataRequest
        , label = showCSVFile model
        }


loadRegsButtonClr : Model -> Color
loadRegsButtonClr model =
    case model.csvContent of
        Just _ ->
            if model.csvLoaded then
                lightGreen

            else
                lightGrey

        Nothing ->
            grey



------------------------------------------------------------------------------------------------------------------
-- Registers Menu


newRegisterTab : List records -> List (IndexedColumn records Msg) -> Element Msg
newRegisterTab dt cl =
    indexedTable
        [ Background.color grey
        , width fill
        , height fill
        , Font.center
        ]
        { data = dt
        , columns = cl
        }


modDataColumns : Model -> List (IndexedColumn ModDataUpdate Msg)
modDataColumns model =
    [ modNameColumn
    , modRegTypeColumn
    , modAddressColumn
    , modValueTypeColumn
    , modValueColumn
    , modUidColumn
    , modDescriptionColumn
    , readWriteColumn model
    , selectColumn model
    ]


modNameColumn : IndexedColumn ModDataUpdate Msg
modNameColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Name"
    , width = fillPortion 1
    , view = \i md -> viewCell i md.mduModData.modName
    }


modRegTypeColumn : IndexedColumn ModDataUpdate Msg
modRegTypeColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Register Type"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| getRegType md.mduModData.modRegType
    }


modAddressColumn : IndexedColumn ModDataUpdate Msg
modAddressColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Register Address"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.mduModData.modAddress
    }


modValueTypeColumn : IndexedColumn ModDataUpdate Msg
modValueTypeColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Value Type"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| getModValueType md.mduModData.modValue
    }


modValueColumn : IndexedColumn ModDataUpdate Msg
modValueColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Value"
    , width = fillPortion 1
    , view = \idx md -> viewModValueColumn idx md
    }


viewModValueColumn : Int -> ModDataUpdate -> Element Msg
viewModValueColumn idx md =
    case md.mduRW of
        Read ->
            viewReadModValue idx md

        Write ->
            viewWriteModValue idx md.mduModData


viewReadModValue : Int -> ModDataUpdate -> Element Msg
viewReadModValue idx md =
    viewCell idx <|
        Maybe.withDefault "Nothing" <|
            getModValue md.mduModData.modValue


viewWriteModValue : Int -> ModData -> Element Msg
viewWriteModValue idx md =
    el
        [ Background.color <| tableCellColor idx
        , Font.center
        ]
    <|
        Input.text
            [ Background.color <| tableCellColor idx
            , Font.color greyWhite
            , Border.width 1
            , height <| px 38

            -- 11 is a magic number here :(
            , paddingXY 0 11
            , focused []
            ]
            { onChange = ChangeModDataValue idx
            , text = Maybe.withDefault "" <| getModValue md.modValue
            , placeholder = Nothing
            , label = Input.labelHidden "Value Input"
            }


modUidColumn : IndexedColumn ModDataUpdate Msg
modUidColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Unit Id"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.mduModData.modUid
    }


modDescriptionColumn : IndexedColumn ModDataUpdate Msg
modDescriptionColumn =
    { header = el [ height <| px 38 ] <| el [ alignLeft, centerY ] <| text "Description"
    , width = fillPortion 4
    , view = \i md -> viewDescCell i md.mduModData.modDescription
    }


readWriteColumn : Model -> IndexedColumn ModDataUpdate Msg
readWriteColumn model =
    { header =
        el
            [ height <| px 38
            , Font.color greyWhite
            ]
        <|
            readWriteButton model.readWriteAll <|
                Just <|
                    ToggleWriteAll <|
                        flipRW model.readWriteAll
    , width = px 50
    , view = \i md -> viewReadWriteCell model i md
    }


readWriteButton : ReadWrite -> Maybe Msg -> Element Msg
readWriteButton rw msg =
    Input.button
        [ Background.color <| rwButtonBGClr rw
        , centerY
        , padding 3
        , focused []
        ]
        { onPress = msg
        , label = readWriteButtonText rw
        }


rwButtonBGClr : ReadWrite -> Color
rwButtonBGClr rw =
    case rw of
        Read ->
            blueSapphire

        Write ->
            fireBrick


readWriteButtonText : ReadWrite -> Element Msg
readWriteButtonText rw =
    case rw of
        Read ->
            text "Read"

        Write ->
            text "Write"


selectColumn : Model -> IndexedColumn ModDataUpdate Msg
selectColumn model =
    { header =
        el
            [ height <| px 38 ]
        <|
            selectCheckbox SelectAllChecked model.selectAllCheckbox
    , width = px 30
    , view = \i md -> viewCheckedCell i md.mduSelected
    }


selectCheckbox : (Bool -> Msg) -> Bool -> Element Msg
selectCheckbox msg flag =
    Input.checkbox
        [ alignLeft
        , centerY
        ]
        { onChange = msg
        , icon = Input.defaultCheckbox
        , checked = flag
        , label = Input.labelHidden "Select Checkbox"
        }


headerTextAttr : List (Attribute Msg)
headerTextAttr =
    [ centerX, centerY ]


viewCell : Int -> String -> Element Msg
viewCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
        (el [ centerX, centerY ] <| text str)


viewDescCell : Int -> String -> Element Msg
viewDescCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
        (el [ alignLeft, centerY ] <| text str)


viewCheckedCell : Int -> Bool -> Element Msg
viewCheckedCell idx selected =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
    <|
        selectCheckbox (ModDataChecked idx) selected


viewReadWriteCell : Model -> Int -> ModDataUpdate -> Element Msg
viewReadWriteCell model idx md =
    case model.activeTab of
        ModDataTab ->
            viewReadWriteModDataCell idx md

        HoldingRegistersTab ->
            none

        _ ->
            none


viewReadWriteModDataCell : Int -> ModDataUpdate -> Element Msg
viewReadWriteModDataCell idx md =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
    <|
        if writeableReg md.mduModData then
            readWriteButton md.mduRW <|
                Just <|
                    ModDataWrite idx <|
                        flipRW md.mduRW

        else
            none


tableCellColor : Int -> Color
tableCellColor idx =
    if modBy 2 idx == 0 then
        lightGrey

    else
        grey


inputRegistersTab : Element Msg
inputRegistersTab =
    newRegisterTab [] []


holdingRegistersTab : Element Msg
holdingRegistersTab =
    newRegisterTab [] []


modDataTab : Model -> Element Msg
modDataTab model =
    newRegisterTab model.modDataUpdate <| modDataColumns model


heartbeatTab : Model -> Element Msg
heartbeatTab model =
    column
    []
    [ text "A line above the heavens"
    , row
        []
        [ text "An into to a dropdown "
        , dropdown
            []
            model.dummyDropdown
        , text " some final words"
        ]
    , text "a line on the abyss, now even more extended"
    ]


------------------------------------------------------------------------------------------------------------------
-- Settings Tab

settingsTab : Model -> Element Msg
settingsTab model =
    el
        [ Background.color grey
        , width fill
        , alignTop
        , alignLeft
        , paddingXY 20 10
        ]
    <|
        renderSettings SetActiveSetting model.settings

------------------------------------------------------------------------------------------------------------------
-- Command Area


commandArea : Model -> Element Msg
commandArea model =
    case model.activeTab of
        ConnectMenu ->
            none

        ImportMenu ->
            none

        InputRegistersTab ->
            none

        HoldingRegistersTab ->
            none

        ModDataTab ->
            modDataCommand model

        HeartbeatTab ->
            none

        SettingsTab ->
            none


modDataCommand : Model -> Element Msg
modDataCommand model =
    if model.selectSome then
        column
            [ Background.color grey
            , width <| px 300
            , height fill
            , Border.widthXY 1 0
            , Border.color lightGrey
            , padding 10
            ]
            [ updateSelectedButton model ]

    else
        none


updateSelectedButton : Model -> Element Msg
updateSelectedButton model =
    Input.button
        [ Background.color lightGrey
        , width fill
        , Font.center
        , Font.color greyWhite
        , paddingXY 0 10
        ]
        { onPress = Just <| RefreshRequest model.modDataUpdate
        , label = text "Update Selected"
        }



------------------------------------------------------------------------------------------------------------------
-- Status Bar
-- An expandable status bar at the bottom of the page


notifications : Model -> Element Msg
notifications model =
    column
        [ width fill
        ]
        [ expandButton model.statusBarState ExpandStatus
        , renderNotifications
            model.timeZone
            ExpandNotification
            model.statusBarState
            model.notifications
        ]
