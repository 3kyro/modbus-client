module View exposing (view)

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
        , px
        , rotate
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
import RegisterTab exposing (regNav, renderOutput, sendRegRequestButton)
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
        [ connectButton model
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
        [ updateSelectedButton model
        , loadCSVButton
        , loadRegisterTableButton model
        ]


heartbeatNavModule : Model -> Element Msg
heartbeatNavModule model =
    none


settingsNavModule : Model -> Element Msg
settingsNavModule model =
    none



------------------------------------------------------------------------------------------------------------------
-- Logo Module
------------------------------------------------------------------------------------------------------------------


renderLogoModule : Model -> Element Msg
renderLogoModule model =
    case model.activeTab of
        _ ->
            simpleLogo


simpleLogo : Element Msg
simpleLogo =
    el
        [ Background.color background
        , Font.size 20
        , Font.color lightGrey
        , alignLeft
        , alignBottom
        ]
    <|
        text "Modbus Serve"



------------------------------------------------------------------------------------------------------------------
-- Info Module
------------------------------------------------------------------------------------------------------------------


renderInfoModule : Model -> Element Msg
renderInfoModule model =
    case model.activeTab of
        ConnectMenu ->
            connectIsland model

        RegistersTab ->
            renderOutput model.regResponse

        ModDataTab ->
            newRegisterTab model.modDataUpdate <| modDataColumns model

        _ ->
            none



------------------------------------------------------------------------------------------------------------------
-- Connect
------------------------------------------------------------------------------------------------------------------


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
        , label = Input.labelLeft [ Font.color white ] <| el [ width <| px 100 ] (text "Timeout (s)")
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


loadRegisterTableButton : Model -> Element Msg
loadRegisterTableButton model =
    case model.csvContent of
        Just _ ->
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
                , label = text <| csvLoadButtonText model.csvLoaded
                }

        Nothing ->
            none


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
        , Font.center
        ]
        { data = dt
        , columns = cl
        }


modDataColumns : Model -> List (IndexedColumn ModDataUpdate Msg)
modDataColumns model =
    [ selectColumn model
    , modNameColumn
    , modRegTypeColumn
    , modAddressColumn
    , modValueTypeColumn
    , modValueColumn (Just ChangeModDataValue)
    , modUidColumn
    , modDescriptionColumn
    , readWriteColumn model
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
            [ height <| px 38
            , paddingXY 10 0
            ]
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
        , paddingXY 10 0
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
        , width fill
        , height <| px 38
        , Font.center
        , Font.color greyWhite
        , paddingXY 0 10
        , focused []
        ]
        { onPress = Just <| RefreshRequest model.modDataUpdate
        , label = text "Update Selected"
        }



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
