module View exposing (view, page)

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
    , maximum
    , table
    , htmlAttribute
    , Color
    )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes

import Types exposing
    ( Msg(..)
    , ActiveMenu (..)
    , Model
    , showStatus
    , Status (..)
    , ActiveTable(..)
    , showConnectStatus
    , showLoadedFileName
    , ConnectStatus(..)
    )
import Types.IpAddress exposing
    ( IpAddress
    , IpAddressByte (..)
    , showIpAddressByte
    )
import Palette exposing
    ( dimGrey
    , grey
    , lightGrey
    , white
    , maximumBluePurple
    , maximumBluePurpleLight
    , darkBluePurple
    , lightGreen
    , smallFont
    )

view : Model -> Html Msg
view model = layout [] <| page model

page : Model -> Element Msg
page model =
    column
        [ width fill
        , height fill
        , smallFont
        ]
        [ mainCell model
        , statusBar model
        ]



------------------------------------------------------------------------------------------------------------------
-- Main Cell

mainCell : Model -> Element Msg
mainCell model =
    row
        [ width fill
        , height fill ]
        [ registerCell model
        , commandBar model
        ]


------------------------------------------------------------------------------------------------------------------
-- Registers Cell

registerCell : Model -> Element Msg
registerCell model =
    column
        [ height fill
        , width fill
        ]
        [ tableSelectBar
        , tablesCell model
        ]

------------------------------------------------------------------------------------------------------------------
-- Tables Select Bar

tableSelectBar : Element Msg
tableSelectBar =
    row
        [ Background.color dimGrey
        , alignTop
        , width fill
        , height <| px 30
        , paddingXY 10 0
        , spacing 0
        ]
        [ inputRegistersButton
        , holdingRegistersButton
        , registerTableButton
        , heartbeatButton
        ]

newSelectButton : String -> Maybe Msg -> Element Msg
newSelectButton str action =
    Input.button
        [
         focused
            [ Background.color grey
            , Font.color white
            ]
        , mouseOver [ Font.color white ]
        , Border.width 0
        , height fill
        , paddingXY 10 0
        , Font.color lightGrey
        ]
        { onPress = action
        , label = text str
        }

inputRegistersButton : Element Msg
inputRegistersButton =
    newSelectButton "Input Registers" Nothing
    
holdingRegistersButton : Element Msg
holdingRegistersButton =
    newSelectButton "Holding Registers" Nothing

registerTableButton : Element Msg
registerTableButton =
    newSelectButton "Register Table" Nothing

heartbeatButton : Element Msg
heartbeatButton =
    newSelectButton "Heartbeat Signals" Nothing

------------------------------------------------------------------------------------------------------------------
-- Tables Cell

tablesCell : Model -> Element Msg
tablesCell model =
    case model.activeTable of
        InputRegistersTable -> inputRegistersTable model
        HoldingRegistersTable -> holdingRegistersTable model
        ModDataTable -> modDataTable model
        HeartbeatTable -> heartbeatTable model

newRegisterTable : Model -> Element Msg
newRegisterTable model =
    table
        [ Background.color grey
        , width fill
        , height fill
        ]
        { data = []
        , columns = []
        }

inputRegistersTable : Model -> Element Msg
inputRegistersTable model = newRegisterTable model

holdingRegistersTable : Model -> Element Msg
holdingRegistersTable model = newRegisterTable model

modDataTable : Model -> Element Msg
modDataTable model = newRegisterTable model

heartbeatTable : Model -> Element Msg
heartbeatTable model = newRegisterTable model

------------------------------------------------------------------------------------------------------------------
-- Command Bar

commandBar : Model -> Element Msg
commandBar model =
    column
        [ width ( fill |> maximum 500 )
        , height fill
        , Background.color darkBluePurple
        , Font.color white ]
        [ connectionButton
        , connectMenu model
        , importRegTableButton
        , importMenu model
        ]

newCommandButton : String -> Maybe Msg -> Element Msg
newCommandButton str action =
    Input.button
        [
         focused
            [ Background.color maximumBluePurpleLight
            , Font.color white
            ]
        , mouseOver [ Font.color white ]
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        , height <| px 30
        , width fill
        , paddingXY 0 0
        , Font.color lightGrey
        , Font.center
        ]
        { onPress = action
        , label = text str
        }

connectionButton : Element Msg
connectionButton =
    newCommandButton "Connection" <| Just <|ChangeActiveMenu ConnectMenu
connectMenu : Model -> Element Msg
connectMenu model =
    case model.activeMenu of
        ConnectMenu -> connectionActiveMenu model
        _ -> none

importRegTableButton : Element Msg
importRegTableButton =
    newCommandButton "Import" <| Just <| ChangeActiveMenu ImportRegistersMenu

importMenu : Model -> Element Msg
importMenu model =
    case model.activeMenu of
        ImportRegistersMenu -> importActiveMenu model
        _ -> none

------------------------------------------------------------------------------------------------------------------
-- Connect Menu

connectionActiveMenu : Model -> Element Msg
connectionActiveMenu model =
    column
        [ width fill
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
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
        [ width fill
        , spacing 5
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
        [
          width <| px 70
        , Background.color darkBluePurple
        , htmlAttribute <| Html.Attributes.maxlength 3
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
        , Background.color darkBluePurple
        , htmlAttribute <| Html.Attributes.maxlength 5
        , focused [ Border.glow white 1 ]
        ]
        { onChange = ChangePort
        , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.socketPort
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ width <| px 100 ] (text "Port")
        }

timeout : Model -> Element Msg
timeout model =
    Input.text
        [ width <| px 70
        , Background.color darkBluePurple
        , htmlAttribute <| Html.Attributes.maxlength 5
        , focused [ Border.glow white 1 ]
        ]
        { onChange = ChangeTimeout
        , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.timeout
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ width <| px 100 ] (text "Timeout (ms)")
        }

connectButton : Model -> Element Msg
connectButton model =
    Input.button
        [ Background.color <| connectButtonBgd model
        , mouseOver [ Font.color white ]
        , height <| px 30
        , width fill
        , paddingXY 0 0
        , Font.color lightGrey
        , Font.center
        , focused []
        ]
        { onPress = Just ConnectRequest
        , label = text <| showConnectStatus model.connectStatus
        }

connectButtonBgd : Model -> Color
connectButtonBgd model =
    case model.connectStatus of
        Connect -> maximumBluePurpleLight
        Connecting -> maximumBluePurpleLight
        Connected -> lightGreen
        Disconnecting -> maximumBluePurple


disconnectButton : Model -> Element Msg
disconnectButton model =
    Input.button
        [ Background.color <| disconnectButtonBgd model
        , mouseOver [ Font.color white ]
        , height <| px 30
        , width fill
        , paddingXY 0 0
        , Font.color lightGrey
        , Font.center
        , focused []
        ]
        { onPress = Just DisconnectRequest
        , label = text "Disconnect"
        }

disconnectButtonBgd : Model -> Color
disconnectButtonBgd model =
    case model.connectStatus of
        Connect -> darkBluePurple
        Connecting -> darkBluePurple
        Connected -> maximumBluePurpleLight
        Disconnecting -> darkBluePurple

------------------------------------------------------------------------------------------------------------------
-- Import Menu

importActiveMenu : Model -> Element Msg
importActiveMenu model =
    column
        [ width fill
        , Border.widthEach
            { bottom = 1
            , left = 0
            , right = 0
            , top = 0
            }
        , spacing 20
        , paddingXY 10 20
        ]
        [ loadCSVButton
        , showCSVFile model
        , loadRegisterTableButton
        -- , showNumOfLoadedRegs
        ]

loadCSVButton : Element Msg
loadCSVButton =
    Input.button
        [ Background.color maximumBluePurpleLight
        , mouseOver [ Font.color white ]
        , height <| px 30
        , width fill
        , paddingXY 0 0
        , Font.color lightGrey
        , Font.center
        , focused []
        ]
        { onPress = Just CsvRequested
        , label = text "Load CSV File"
        }

showCSVFile : Model -> Element Msg
showCSVFile model =
    el
        []
        ( text <| showLoadedFileName model )
loadRegisterTableButton : Element Msg
loadRegisterTableButton =
    Input.button
        [ Background.color maximumBluePurpleLight
        , mouseOver [ Font.color white ]
        , height <| px 30
        , width fill
        , paddingXY 0 0
        , Font.color lightGrey
        , Font.center
        , focused []
        ]
        { onPress = Just ModDataRequest
        , label = text "Load Register Table"
        }


------------------------------------------------------------------------------------------------------------------
-- Status Bar

statusBar : Model -> Element Msg
statusBar model =
    row
        [ Background.color maximumBluePurple
        , width fill
        , height <| px 30
        , alignBottom
        ]
        [ text <| showStatus model.status ]


