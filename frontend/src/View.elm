module View exposing (view)

import Element exposing
    ( Element
    , el
    , text
    , row
    , column
    , layout
    , fill
    , width
    , height
    , px
    , paddingXY
    , spacing
    , focused
    , mouseOver
    , none
    , alignBottom
    , alignTop
    , alignLeft
    , centerX
    , centerY
    , indexedTable
    , htmlAttribute
    , Color
    , IndexedColumn
    , Attribute
    , fillPortion
    )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes

import Types exposing
    ( Msg(..)
    , ActiveTab (..)
    , Model
    , showStatus
    , Status (..)
    , ConnectStatus(..)
    , showConnectStatus
    , ModData
    , getRegType
    , getModValueType
    , getModValue
    )
import Types.IpAddress exposing
    ( IpAddress
    , IpAddressByte (..)
    , showIpAddressByte
    )
import Palette exposing
    ( darkGrey
    , grey
    , lightGrey
    , greyWhite
    , white
    , lightGreen
    , smallFont
    , scampi
    , slateGrey
    )

view : Model -> Html Msg
view model = layout [] <| page model

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
        , statusExpanded
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
        , height <| px 30
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
    if model.activeTab == table
    then grey
    else darkGrey

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
        ConnectMenu -> connectTab model
        ImportMenu -> importTab model
        InputRegistersTable -> inputRegistersTable
        HoldingRegistersTable -> holdingRegistersTable
        ModDataTable -> modDataTable model
        HeartbeatTable -> heartbeatTable

------------------------------------------------------------------------------------------------------------------
-- Connect Menu

connectTab : Model -> Element Msg
connectTab model =
    el
        [ Background.color grey
        , width fill
        , height fill
        ]
        <| connectIsland model

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

ipAddressInput :  IpAddressByte -> IpAddress -> Element Msg
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
        , height <| px 30
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
        Connect -> lightGrey
        Connecting -> lightGrey
        Connected -> lightGreen
        Disconnecting -> lightGrey

disconnectButton : Model -> Element Msg
disconnectButton model =
    Input.button
        [ Background.color <| disconnectButtonBgd model
        , mouseOver [ Font.color white ]
        , height <| px 30
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
        Connect -> grey
        Connecting -> grey
        Connected -> lightGrey
        Disconnecting -> grey

------------------------------------------------------------------------------------------------------------------
-- Import Menu

importTab : Model -> Element Msg
importTab model =
    el
        [ Background.color grey
        , width fill
        , height fill
        ]
        <| importActiveTab model

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
        , height <| px 30
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
        Nothing -> none
        Just filename ->
            el
                [ Font.color white, centerX ]
                ( text <| csvLoadButtonText model.csvLoaded filename  )

csvLoadButtonText : Bool -> String -> String
csvLoadButtonText flag str =
    if flag
    then str ++ " registers imported!"
    else "Import registers from " ++ str


loadRegisterTableButton : Model ->  Element Msg
loadRegisterTableButton model =
    Input.button
        [ Background.color <| loadRegsButtonClr model
        , mouseOver [ Font.color white ]
        , height <| px 30
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
        if model.csvLoaded
        then lightGreen
        else lightGrey
       Nothing -> grey


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

modDataColumns : Model -> List ( IndexedColumn ModData Msg )
modDataColumns model =
    [ modNameColumn
    , modRegTypeColumn
    , modAddressColumn
    , modValueTypeColumn
    , modValueColumn
    , modUidColumn
    , modDescriptionColumn
    , selectColumn model
    ]

modNameColumn : IndexedColumn ModData Msg
modNameColumn =
    { header = el [ height <| px 30 ] <| el headerTextAttr <| text "Name"
    , width = fillPortion 1
    , view = \i md -> viewCell i md.modName
    }

modRegTypeColumn : IndexedColumn ModData Msg
modRegTypeColumn =
    { header = el [ height <| px 30 ] <| el headerTextAttr <| text "Register Type"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| getRegType md.modRegType
    }

modAddressColumn : IndexedColumn ModData Msg
modAddressColumn =
    { header = el [ height <| px 30 ] <| el headerTextAttr <| text "Register Address"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.modAddress
    }

modValueTypeColumn : IndexedColumn ModData Msg
modValueTypeColumn =
    { header = el [ height <| px 30 ] <| el headerTextAttr <| text "Value Type"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| getModValueType md.modValue
    }

modValueColumn : IndexedColumn ModData Msg
modValueColumn =
    { header = el [ height <| px 30 ] <| el headerTextAttr <| text "Value"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| Maybe.withDefault "Nothing" <| getModValue md.modValue
    }

modUidColumn : IndexedColumn ModData Msg
modUidColumn =
    { header = el [ height <| px 30 ] <| el headerTextAttr <| text "Unit Id"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.modUid
    }

modDescriptionColumn : IndexedColumn ModData Msg
modDescriptionColumn =
    { header = el [ height <| px 30 ] <| el [alignLeft , centerY ] <| text "Description"
    , width = fillPortion 4
    , view = \i md -> viewDescCell i md.modDescription
    }

selectColumn : Model -> IndexedColumn ModData Msg
selectColumn model =
    { header =
        el
            [ height <| px 30 ] <| el [alignLeft , centerY ]
            <| Input.checkbox
                []
                { onChange = SelectAllChecked
                , icon = Input.defaultCheckbox
                , checked = model.selectAllCheckbox
                , label = Input.labelHidden "Select all"
                }
    , width = px 30
    , view = \i md -> viewCheckedCell i md.selected
    }

headerTextAttr : List (Attribute Msg)
headerTextAttr =
    [ centerX , centerY ]

viewCell : Int -> String -> Element Msg
viewCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 30
        , Font.center
        ]
        ( el [centerX , centerY ] <| text str )

viewDescCell : Int -> String -> Element Msg
viewDescCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 30
        , Font.center
        ]
        ( el [alignLeft, centerY ] <| text str )

viewCheckedCell : Int -> Bool -> Element Msg
viewCheckedCell idx selected =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 30
        , Font.center
        ]
        <| Input.checkbox
                [alignLeft , centerY]
                { onChange = ModDataChecked idx
                , icon = Input.defaultCheckbox
                , checked = selected
                , label = Input.labelHidden "Select Field"
                }

tableCellColor : Int -> Color
tableCellColor idx =
    if modBy 2 idx == 0
    then lightGrey
    else grey

inputRegistersTable : Element Msg
inputRegistersTable = newRegisterTable [] []

holdingRegistersTable : Element Msg
holdingRegistersTable = newRegisterTable [] []

modDataTable : Model -> Element Msg
modDataTable model = newRegisterTable model.modData <| modDataColumns model

heartbeatTable : Element Msg
heartbeatTable = newRegisterTable [] []

------------------------------------------------------------------------------------------------------------------
-- Command Area

commandArea : Model -> Element Msg
commandArea model =
    case model.activeTab of
        ConnectMenu -> none
        ImportMenu -> none
        InputRegistersTable -> none
        HoldingRegistersTable -> none
        ModDataTable -> modDataCommand model
        HeartbeatTable -> none

modDataCommand : Model -> Element Msg
modDataCommand model =
    if model.selectSome
    then
        column
            [ Background.color scampi
            , width <| px 300
            , height fill
            ] []
    else none
------------------------------------------------------------------------------------------------------------------
-- Status Bar

statusBar : Model -> Element Msg
statusBar model =
    row
        [ Background.color slateGrey
        , width fill
        , height <| px 30
        , alignBottom
        ]
        [ text <| showStatus model.status ]


statusExpanded : Element Msg
statusExpanded = none