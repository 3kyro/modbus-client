module View exposing (view)

import Dropdown exposing (..)
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
import ModData
    exposing
        ( ModData
        , ModDataUpdate
        , getModValue
        , getModValueType
        , getModValueUpdate
        , isWriteableReg
        , modAddressColumn
        , modDescriptionColumn
        , modNameColumn
        , modRegTypeColumn
        , modUidColumn
        , modValueColumn
        , modValueTypeColumn
        , showRegType
        , tableCellColor
        )
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
import ReadWrite
    exposing
        ( ReadWrite(..)
        , flipRW
        , readWriteButton
        )
import RegisterTab exposing (renderRegistersTab)
import Settings
    exposing
        ( renderSettings
        )
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , Model
        , Msg(..)
        , showConnectStatus
        )
import Types.IpAddress
    exposing
        ( IpAddress
        , IpAddressByte(..)
        , showIpAddressByte
        )


view : Model -> Html Msg
view model =
    layout
        [ Background.color grey
        , width fill
        , height fill
        , smallFont
        ]
    <|
        page model


page : Model -> Element Msg
page model =
    column
        [ width fill
        , height fill
        ]
        [ mainModule model
        , notifications model
        ]


mainModule : Model -> Element Msg
mainModule model =
    row
        [ width fill
        , height fill
        , padding 50
        , spacing 50
        ]
        [ left model
        , right model
        ]


left : Model -> Element Msg
left model =
    column
        [ width <| fillPortion 1
        , height fill
        , spacing 50
        ]
        [ navigationModule model
        , manipulationModule model
        ]


right : Model -> Element Msg
right model =
    infoModule model


navigationModule : Model -> Element Msg
navigationModule model =
    el
        [ Background.color blueSapphire
        , width fill
        , height <| fillPortion 1
        , sh
        ]
    <|
        renderNavModule model


manipulationModule : Model -> Element Msg
manipulationModule model =
    el
        [ Background.color blueSapphire
        , width fill
        , height <| fillPortion 4
        , sh
        ]
    <|
        text "manipulation"


infoModule : Model -> Element Msg
infoModule model =
    el
        [ Background.color blueSapphire
        , width <| fillPortion 4
        , height fill
        , sh
        ]
    <|
        text "info"


sh : Attribute Msg
sh =
    Border.shadow
        { offset = ( 0, 0 )
        , size = 0.2
        , blur = 100
        , color = darkGrey
        }



------------------------------------------------------------------------------------------------------------------
-- Navigation Buttons
------------------------------------------------------------------------------------------------------------------


renderNavModule : Model -> Element Msg
renderNavModule model =
    column
        [ Background.color darkGrey
        , width fill
        , height fill
        , spacing 0
        ]
        [ firstNavRow model
        , secondNavRow model
        ]


firstNavRow : Model -> Element Msg
firstNavRow model =
    row
        [ width fill
        , height fill
        ]
        [ connectTabButton model
        , importRegisterTabButton model
        , registersTabButton model
        ]


secondNavRow : Model -> Element Msg
secondNavRow model =
    row
        [ width fill
        , height fill
        ]
        [ registerTableTabButton model
        , heartbeatTabButton model
        , settingsTabButton model
        ]



-- Creates a new navigation button selection button


newNavigationButton : Model -> String -> ActiveTab -> Element Msg
newNavigationButton model str tab =
    Input.button
        [ Background.color <| selectTabButtonBgdColor model tab
        , focused [ Font.color white ]
        , mouseOver [ Font.color white ]
        , Border.width 0
        , height fill
        , width <| fillPortion 1
        , paddingXY 10 0
        , Font.color greyWhite
        , Font.center
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
    newNavigationButton model "Connect" ConnectMenu


importRegisterTabButton : Model -> Element Msg
importRegisterTabButton model =
    newNavigationButton model "Import" ImportMenu


registersTabButton : Model -> Element Msg
registersTabButton model =
    newNavigationButton model "Registers" RegistersTab


registerTableTabButton : Model -> Element Msg
registerTableTabButton model =
    newNavigationButton model "Table" ModDataTab


heartbeatTabButton : Model -> Element Msg
heartbeatTabButton model =
    newNavigationButton model "Heartbeat" HeartbeatTab


settingsTabButton : Model -> Element Msg
settingsTabButton model =
    newNavigationButton model "Settings" SettingsTab



------------------------------------------------------------------------------------------------------------------
-- Manipulation Module
------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------
-- Manipulation Module
------------------------------------------------------------------------------------------------------------------


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

        RegistersTab ->
            registersTab model

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
                Connect ->
                    Just ConnectRequest

                _ ->
                    Nothing
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
    , modValueColumn (Just ChangeModDataValue)
    , modUidColumn
    , modDescriptionColumn
    , readWriteColumn model
    , selectColumn model
    ]


readWriteColumn : Model -> IndexedColumn ModDataUpdate Msg
readWriteColumn model =
    { header =
        el
            [ height <| px 38
            , Font.color greyWhite
            ]
        <|
            readWriteButton
                model.readWriteAll
                blueSapphire
                fireBrick
            <|
                Just <|
                    ToggleWriteAll <|
                        flipRW model.readWriteAll
    , width = px 50
    , view = \i md -> viewReadWriteCell model i md
    }


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
        if isWriteableReg md.mduModData.modRegType then
            readWriteButton md.mduRW
                blueSapphire
                fireBrick
            <|
                Just <|
                    ModDataWrite idx <|
                        flipRW md.mduRW

        else
            none


registersTab : Model -> Element Msg
registersTab model =
    renderRegistersTab model


holdingRegistersTab : Element Msg
holdingRegistersTab =
    newRegisterTab [] []


modDataTab : Model -> Element Msg
modDataTab model =
    newRegisterTab model.modDataUpdate <| modDataColumns model


heartbeatTab : Model -> Element Msg
heartbeatTab model =
    none



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

        RegistersTab ->
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
