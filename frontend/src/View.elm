module View exposing (view)

import Deque exposing (isEmpty)
import Dropdown exposing (..)
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , IndexedColumn
        , alignBottom
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
        , moveDown
        , moveLeft
        , none
        , padding
        , paddingXY
        , paragraph
        , px
        , rotate
        , row
        , scrollbarX
        , scrollbarY
        , scrollbars
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Heartbeat exposing (hbHelpText, heartBeatInfoModule, heartBeatNav)
import Html exposing (Html)
import Html.Attributes
import ModData
    exposing
        ( ModData
        , ModDataUpdate
        , getModValue
        , getModValueUpdate
        , isWriteableReg
        , modAddressColumn
        , modDescriptionColumn
        , modNameColumn
        , modRegTypeColumn
        , modUidColumn
        , modValueColumn
        , modValueTypeColumn
        , readWriteColumn
        , selectColumn
        , showModValueType
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
        ( background
        , black
        , blueSapphire
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
import RegisterTab
    exposing
        ( regNav
        , registersHelpModule
        , renderOutput
        , sendRegRequestButton
        )
import Settings
    exposing
        ( renderSettings
        )
import Types
    exposing
        ( ActiveTab(..)
        , ConnectActiveTab(..)
        , ConnectStatus(..)
        , Model
        , Msg(..)
        , OS(..)
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
        [ Background.color background
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
        , scrollbarY
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
        , scrollbarY
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
        , helpModule model
        , logoModule model
        ]


right : Model -> Element Msg
right model =
    infoModule model


navigationModule : Model -> Element Msg
navigationModule model =
    el
        [ width fill
        , height <| fillPortion 1
        ]
    <|
        renderNavModule model


manipulationModule : Model -> Element Msg
manipulationModule model =
    el
        [ width fill
        , height <| fillPortion 4
        ]
    <|
        renderManModule model


helpModule : Model -> Element Msg
helpModule model =
    el
        [ width fill
        , height <| fillPortion 1
        ]
    <|
        renderHelpModule model


logoModule : Model -> Element Msg
logoModule model =
    el
        [ Background.color background
        , width fill
        , height <| fillPortion 1
        ]
    <|
        renderLogoModule model


infoModule : Model -> Element Msg
infoModule model =
    el
        [ Background.color grey
        , width <| fillPortion 4
        , height fill
        , scrollbarX
        , scrollbarY
        ]
    <|
        renderInfoModule model



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
        , registersTabButton model
        , registerTableTabButton model
        ]


secondNavRow : Model -> Element Msg
secondNavRow model =
    row
        [ width fill
        , height fill
        ]
        [ heartbeatTabButton model
        , settingsTabButton model
        , emptyTabButton model
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


emptyTabButton : Model -> Element Msg
emptyTabButton model =
    el
        [ Border.width 0
        , height fill
        , width <| fillPortion 1
        , paddingXY 10 0
        ]
        none



------------------------------------------------------------------------------------------------------------------
-- Manipulation Module
------------------------------------------------------------------------------------------------------------------


renderManModule : Model -> Element Msg
renderManModule model =
    case model.activeTab of
        ConnectMenu ->
            connectNavModule model

        RegistersTab ->
            registersNavModule model

        ModDataTab ->
            tableNavModule model

        HeartbeatTab ->
            heartbeatNavModule model

        SettingsTab ->
            settingsNavModule model


connectNavModule : Model -> Element Msg
connectNavModule model =
    column
        [ spacing 10
        , width fill
        ]
        [ connActiveTabButton model TCPTab
        , connActiveTabButton model RTUTab
        , connectButton model
        , disconnectButton model
        ]


registersNavModule : Model -> Element Msg
registersNavModule model =
    el
        [ width fill ]
    <|
        regNav model


tableNavModule : Model -> Element Msg
tableNavModule model =
    column
        [ spacing 10
        , width fill
        ]
        [ loadCSVButton
        , updateSelectedButton model
        ]


heartbeatNavModule : Model -> Element Msg
heartbeatNavModule model =
    el
        [ width fill ]
    <|
        heartBeatNav model


settingsNavModule : Model -> Element Msg
settingsNavModule model =
    none


emptySpace : Element Msg
emptySpace =
    el
        [ Background.color background
        , height <| px 38
        , width fill
        , paddingXY 0 0
        ]
        none



------------------------------------------------------------------------------------------------------------------
-- Help Module
------------------------------------------------------------------------------------------------------------------


renderHelpModule : Model -> Element Msg
renderHelpModule model =
    case model.activeTab of
        RegistersTab ->
            registersHelpModule model

        HeartbeatTab ->
            hbHelpModule model

        _ ->
            none


hbHelpModule : Model -> Element Msg
hbHelpModule model =
    paragraph
        [ alignTop
        , Font.color lightGrey
        ]
    <|
        hbHelpText model.hbTypeDd.selected.value



------------------------------------------------------------------------------------------------------------------
-- Logo Module
------------------------------------------------------------------------------------------------------------------


renderLogoModule : Model -> Element Msg
renderLogoModule model =
    column
        [ Background.color background
        , Font.color lightGrey
        , alignLeft
        , alignBottom
        , spacing 10
        ]
        [ el [ Font.size 15 ] <| text "version : 0.1.0"
        , el [ Font.size 30 ] <| text "Modbus Client"
        ]



------------------------------------------------------------------------------------------------------------------
-- Info Module
------------------------------------------------------------------------------------------------------------------


renderInfoModule : Model -> Element Msg
renderInfoModule model =
    case model.activeTab of
        ConnectMenu ->
            connectIsland model

        RegistersTab ->
            if List.isEmpty model.regResponse then
                none

            else
                renderOutput model.regResponse

        ModDataTab ->
            if List.isEmpty model.modDataUpdate then
                none

            else
                newRegisterTab model.modDataUpdate <| modDataColumns model

        SettingsTab ->
            settingsTab model

        HeartbeatTab ->
            heartBeatInfoModule model



------------------------------------------------------------------------------------------------------------------
-- Connect
------------------------------------------------------------------------------------------------------------------


connectIsland : Model -> Element Msg
connectIsland model =
    case model.connActiveTab of
        TCPTab ->
            tcpConnectIsland model

        RTUTab ->
            rtuConnectIsland model


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


connActiveTabButton : Model -> ConnectActiveTab -> Element Msg
connActiveTabButton model connTab =
    Input.button
        [ Background.color <| connActiveTabClr model connTab
        , mouseOver [ Font.color white ]
        , height <| px 38
        , width fill
        , paddingXY 0 0
        , Font.color greyWhite
        , Font.center
        , focused []
        ]
        { onPress =
            if model.connActiveTab == connTab then
                Nothing

            else
                Just <| ChangeActiveConnectTab connTab
        , label = text <| connActiveTabText connTab
        }


connActiveTabText : ConnectActiveTab -> String
connActiveTabText connTab =
    case connTab of
        TCPTab ->
            "Modbus TCP"

        RTUTab ->
            "Modbus RTU"


connActiveTabClr : Model -> ConnectActiveTab -> Color
connActiveTabClr model connTab =
    if model.connActiveTab == connTab then
        blueSapphire

    else
        lightGrey



------------------------------------------------------------------------------------------------------------------
-- Connect Rows
------------------------------------------------------------------------------------------------------------------
-- Row structure for single inputs
-- [ spacing 5]
-- [ Main Label , 150 px] [Left label, 50 px] [Input, 70 px] [ Right label, 50 px]


mainLabel : String -> Element Msg
mainLabel str =
    el
        [ width <| px 150
        , Font.color white
        ]
    <|
        text str


leftLabel : String -> Element Msg
leftLabel str =
    el
        [ width <| px 50
        , Font.color white
        , Font.alignRight
        ]
    <|
        text str


connInput :
    Int -- Input max length
    ->
        { onChange : String -> Msg
        , text : String
        , placeholder : Maybe (Input.Placeholder Msg)
        , label : Input.Label Msg
        }
    -> Element Msg
connInput maxLen inputRecord =
    Input.text
        [ width <| px 100
        , Background.color lightGrey
        , htmlAttribute <| Html.Attributes.maxlength maxLen
        , Font.color white
        , focused [ Border.glow white 1 ]
        ]
        inputRecord


rigthLabel : String -> Element Msg
rigthLabel str =
    el
        [ width <| px 50
        , Font.color white
        ]
    <|
        text str



------------------------------------------------------------------------------------------------------------------
-- TCP Connect
------------------------------------------------------------------------------------------------------------------


tcpConnectIsland : Model -> Element Msg
tcpConnectIsland model =
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
        ]


ipaddress : Model -> Element Msg
ipaddress model =
    row
        [ spacing 5
        , Font.color white
        ]
        [ mainLabel "IP Address"
        , leftLabel ""
        , ipAddressInput Byte0 model.ipAddress
        , ipAddressInput Byte1 model.ipAddress
        , ipAddressInput Byte2 model.ipAddress
        , ipAddressInput Byte3 model.ipAddress
        , rigthLabel ""
        ]


ipAddressInput : IpAddressByte -> IpAddress -> Element Msg
ipAddressInput byte ip =
    connInput 3
        { onChange = ChangeIpAddress byte
        , text = showIpAddressByte byte ip
        , placeholder = Nothing
        , label = Input.labelHidden "Byte"
        }


portNum : Model -> Element Msg
portNum model =
    row
        [ spacing 5 ]
        [ mainLabel "Port"
        , leftLabel ""
        , connInput 5
            { onChange = ChangePort
            , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.socketPort
            , placeholder = Nothing
            , label = Input.labelHidden "Port"
            }
        , rigthLabel ""
        ]


timeout : Model -> Element Msg
timeout model =
    row
        [ spacing 5 ]
        [ mainLabel "Timeout"
        , leftLabel ""
        , connInput 5
            { onChange = ChangeTimeout
            , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.timeout
            , placeholder = Nothing
            , label = Input.labelHidden "timeout"
            }
        , rigthLabel "(s)"
        ]



------------------------------------------------------------------------------------------------------------------
-- RTU Connect
------------------------------------------------------------------------------------------------------------------


rtuConnectIsland : Model -> Element Msg
rtuConnectIsland model =
    column
        [ Background.color grey
        , centerX
        , centerY
        , width <| px 500
        , height <| px 500
        , spacing 20
        , paddingXY 10 20
        ]
        [ serialPort model
        , baudrate model
        , stopbits model
        , parity model
        , timeout model
        ]


serialPort : Model -> Element Msg
serialPort model =
    row
        [ spacing 5 ]
        [ mainLabel "Serial Port"
        , leftLabel <| serialPortLabel model.os
        , connInput 20
            { onChange = ChangeSerialPort
            , text = serialPortText model.os model.serialPort
            , placeholder = Nothing
            , label = Input.labelHidden "serial port"
            }
        , rigthLabel ""
        ]


serialPortLabel : OS -> String
serialPortLabel os =
    case os of
        Linux ->
            "ttyS"

        Windows ->
            "COM"

        Other ->
            "Serial Port"


serialPortText : OS -> Maybe String -> String
serialPortText os serialport =
    case os of
        Windows ->
            -- COM port should only be numerical,
            -- so we convert the string to a Maybe Int and back again
            let
                maybeInt =
                    Maybe.andThen String.toInt serialport
            in
            Maybe.withDefault "" <| Maybe.map String.fromInt maybeInt

        _ ->
            -- A linux ttyS can be non numerical,
            -- eg ttySUSB0 for USB-Serial converters
            Maybe.withDefault "" serialport


baudrate : Model -> Element Msg
baudrate model =
    row
        [ spacing 5 ]
        [ mainLabel "Baud Rate"
        , leftLabel ""
        , dropdown
            [ Background.color blueSapphire
            , width <| px 100
            , padding 11
            , height <| px 38
            , Font.center
            , Font.color greyWhite
            , spacing 0
            ]
            model.baudrateDd
        , rigthLabel ""
        ]


stopbits : Model -> Element Msg
stopbits model =
    row
        [ spacing 5 ]
        [ mainLabel "Stop Bits"
        , leftLabel ""
        , dropdown
            [ Background.color blueSapphire
            , width <| px 100
            , padding 11
            , height <| px 38
            , Font.center
            , Font.color greyWhite
            , spacing 0
            ]
            model.stopBitsDd
        , rigthLabel ""
        ]


parity : Model -> Element Msg
parity model =
    row
        [ spacing 5 ]
        [ mainLabel "Parity"
        , leftLabel ""
        , dropdown
            [ Background.color blueSapphire
            , width <| px 100
            , padding 11
            , height <| px 38
            , Font.center
            , Font.color greyWhite
            , spacing 0
            ]
            model.parityDd
        , rigthLabel ""
        ]



------------------------------------------------------------------------------------------------------------------
-- Import
------------------------------------------------------------------------------------------------------------------


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
    indexedTable
        []
        { data = model.modDataUpdate
        , columns =
            []
        }


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


csvLoadButtonText : Bool -> String
csvLoadButtonText flag =
    if flag then
        "Registers Imported"

    else
        "Import Registers"


loadRegsButtonClr : Model -> Color
loadRegsButtonClr model =
    case model.csvContent of
        Just _ ->
            if model.csvLoaded then
                lightGreen

            else
                lightGrey

        Nothing ->
            background



------------------------------------------------------------------------------------------------------------------
-- Registers Menu
------------------------------------------------------------------------------------------------------------------


newRegisterTab : List records -> List (IndexedColumn records Msg) -> Element Msg
newRegisterTab dt cl =
    indexedTable
        [ Background.color grey
        , width fill
        , height fill
        ]
        { data = dt
        , columns = cl
        }


modDataColumns : Model -> List (IndexedColumn ModDataUpdate Msg)
modDataColumns model =
    [ selectColumn SelectAllChecked model.selectAllCheckbox ModDataChecked
    , readWriteColumn ToggleWriteAll model.readWriteAll ModDataWrite
    , modNameColumn
    , modRegTypeColumn
    , modAddressColumn
    , modValueTypeColumn
    , modValueColumn (Just ChangeModDataValue)
    , modUidColumn
    , modDescriptionColumn
    ]


registersTab : Model -> Element Msg
registersTab model =
    none


holdingRegistersTab : Element Msg
holdingRegistersTab =
    newRegisterTab [] []


modDataTab : Model -> Element Msg
modDataTab model =
    newRegisterTab model.modDataUpdate <| modDataColumns model


heartbeatTab : Model -> Element Msg
heartbeatTab model =
    none


updateSelectedButton : Model -> Element Msg
updateSelectedButton model =
    Input.button
        [ Background.color lightGrey
        , mouseOver [ Font.color white ]
        , width fill
        , height <| px 38
        , Font.center
        , Font.color greyWhite
        , paddingXY 0 10
        , focused []
        ]
        { onPress =
            case model.connectStatus of
                Connected ->
                    Just <| RefreshRequest model.modDataUpdate

                _ ->
                    Nothing
        , label = text "Update Selected"
        }



------------------------------------------------------------------------------------------------------------------
-- Settings Tab
------------------------------------------------------------------------------------------------------------------


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
-- Notifications
------------------------------------------------------------------------------------------------------------------


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
