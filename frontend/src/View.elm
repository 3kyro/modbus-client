module View exposing (view)

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
import Palette
    exposing
        ( black
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
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , ModData
        , ModDataUpdate
        , Model
        , Msg(..)
        , ReadWrite(..)
        , Status(..)
        , StatusBarState(..)
        , flipRW
        , getModValue
        , getModValueType
        , getRegType
        , showConnectStatus
        , showStatus
        , writeableReg
        )
import Types.IpAddress
    exposing
        ( IpAddress
        , IpAddressByte(..)
        , showIpAddressByte
        )
import Types exposing (Notification)


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
        , statusBar model
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
        , paddingXY 10 0
        , spacing 0
        ]
        [ connectTabButton model
        , importRegisterTableButton model
        , inputRegistersButton model
        , holdingRegistersButton model
        , registerTableButton model
        , heartbeatButton model
        ]


newSelectButton : Model -> String -> ActiveTab -> Element Msg
newSelectButton model str table =
    Input.button
        [ Background.color <| selectButtonBgdColor model table
        , focused [ Font.color white ]
        , mouseOver [ Font.color white ]
        , Border.width 0
        , height fill
        , paddingXY 10 0
        , Font.color greyWhite
        ]
        { onPress = Just <| ChangeActiveTab table
        , label = text str
        }


selectButtonBgdColor : Model -> ActiveTab -> Color
selectButtonBgdColor model table =
    if model.activeTab == table then
        grey

    else
        darkGrey


connectTabButton : Model -> Element Msg
connectTabButton model =
    newSelectButton model "Connect" ConnectMenu


importRegisterTableButton : Model -> Element Msg
importRegisterTableButton model =
    newSelectButton model "Import" ImportMenu


inputRegistersButton : Model -> Element Msg
inputRegistersButton model =
    newSelectButton model "Input Registers" InputRegistersTable


holdingRegistersButton : Model -> Element Msg
holdingRegistersButton model =
    newSelectButton model "Holding Registers" HoldingRegistersTable


registerTableButton : Model -> Element Msg
registerTableButton model =
    newSelectButton model "Register Table" ModDataTable


heartbeatButton : Model -> Element Msg
heartbeatButton model =
    newSelectButton model "Heartbeat Signals" HeartbeatTable


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

        InputRegistersTable ->
            inputRegistersTable

        HoldingRegistersTable ->
            holdingRegistersTable

        ModDataTable ->
            modDataTable model

        HeartbeatTable ->
            heartbeatTable



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
        , label = Input.labelLeft [ Font.color white ] <| el [ width <| px 100 ] (text "Timeout (ms)")
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
        { onPress = Just ConnectRequest
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
        Connect ->
            grey

        Connecting ->
            grey

        Connected ->
            lightGrey

        Disconnecting ->
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


newRegisterTable : List records -> List (IndexedColumn records Msg) -> Element Msg
newRegisterTable dt cl =
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
        ModDataTable ->
            viewReadWriteModDataCell idx md

        HoldingRegistersTable ->
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


inputRegistersTable : Element Msg
inputRegistersTable =
    newRegisterTable [] []


holdingRegistersTable : Element Msg
holdingRegistersTable =
    newRegisterTable [] []


modDataTable : Model -> Element Msg
modDataTable model =
    newRegisterTable model.modDataUpdate <| modDataColumns model


heartbeatTable : Element Msg
heartbeatTable =
    newRegisterTable [] []



------------------------------------------------------------------------------------------------------------------
-- Command Area


commandArea : Model -> Element Msg
commandArea model =
    case model.activeTab of
        ConnectMenu ->
            none

        ImportMenu ->
            none

        InputRegistersTable ->
            none

        HoldingRegistersTable ->
            none

        ModDataTable ->
            modDataCommand model

        HeartbeatTable ->
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


statusBar : Model -> Element Msg
statusBar model =
    column
        [ width fill
        ]
        [ expandButton model
        , notifications model
        ]


expandButton : Model -> Element Msg
expandButton model =
    Input.button
        [ Background.color darkGrey
        , width fill
        , height <| px 20
        , Font.center
        , focused [ Border.glow black 0 ]
        ]
        { onPress = Just <| ExpandStatus
        , label = expandButtonLabel model
        }


expandButtonLabel : Model -> Element Msg
expandButtonLabel model =
    case model.statusBarState of
        Expanded ->
            -- "▼" \u{25BC}'
            text "▼"

        Retracted ->
            -- "▲" '\u{25B2}'
            text <| String.fromChar '▲'


notifications : Model -> Element Msg
notifications model =
    row
        [ Background.color blueSapphire
        , width fill
        , notificationsHeight model
        , alignBottom
        ]
        [ notificationsTable model ]


notificationsHeight : Model -> Attribute Msg
notificationsHeight model =
    case model.statusBarState of
        Expanded ->
            height <| px 300

        Retracted ->
            height <| px 30


hhmmss : Time.Zone -> Time.Posix -> String
hhmmss zone posix =
    String.fromInt
        (Time.toHour zone posix)
        ++ ":"
        ++ (String.fromInt <| Time.toMinute zone posix)
        ++ ":"
        ++ (String.fromInt <| Time.toSecond zone posix)

notificationsTable : Model -> Element Msg
notificationsTable model =
    indexedTable
        []
        { data = model.notifications
        , columns = notificationColumns model
        }

notificationColumns : Model -> List (IndexedColumn Notification Msg)
notificationColumns model =
    [ notTimeColumn model
    , notheaderColumn
    ]

notTimeColumn : Model -> IndexedColumn Notification Msg
notTimeColumn model =
        { header = none
        , width = px 100
        , view = \_ not -> text <| hhmmss model.timeZone not.time
        }

notheaderColumn : IndexedColumn Notification Msg
notheaderColumn =
        { header = none
        , width = fillPortion 1
        , view = \_ not -> text not.header
        }
