module Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , ModValue (..)
    , ModData
    , getRegType
    , Status (..)
    , showStatus
    , ConnectStatus (..)
    , showConnectStatus
    , ConnectionInfo
    , decodeConnInfo
    , encodeRegister
    , decodeModData
    , ActiveMenu (..)
    , getChangedMenu
    , encodeIpPort
    )


import Http
import Json.Decode as D
import Json.Encode as E

import Types.IpAddress exposing
    ( IpAddress
    , decodeIpAddress
    , showIp
    , IpAddressByte
    , unsafeShowIp
    )

type Msg
    = ReadRegisters (Result Http.Error (List ModData))
    | ReceivedConnectionInfo ( Result Http.Error (Maybe ConnectionInfo))
    | RefreshRequest (List ModData)
    | ConnectRequest
    | ConnectedResponse (Result Http.Error () )
    | ChangeIpAddress (Maybe IpAddress)
    | ChangePort String
    | ChangeTimeout String
    | DisconnectRequest
    | DisconnectedResponse (Result Http.Error () )
    | ChangeActiveMenu ActiveMenu

type alias Model =
    { modData : List ModData
    , status : Status
    , connectStatus : ConnectStatus
    , ipAddress : IpAddress
    , socketPort : Int
    , timeout : Int
    , activeMenu : ActiveMenu
    }
type ConnectStatus
    = Connect
    | Connecting
    | Connected
    | Disconnecting

showConnectStatus : ConnectStatus -> String
showConnectStatus st =
    case st of
        Connect -> "connect"
        Connecting -> "connecting"
        Connected -> "connected"
        Disconnecting -> "disconnecting"

type Status
    = AllGood
    | Loading
    | Bad String
    | BadIpAddress
    | BadPort
    | BadTimeout

showStatus : Status -> String
showStatus status =
    case status of
        AllGood -> "all good"
        Loading -> "getting stuff from the server"
        Bad err -> err
        BadIpAddress -> "Invalid ip address"
        BadPort -> "Bad Port"
        BadTimeout -> "Bad Timeout"

type alias ConnectionInfo =
    { ipAddress : IpAddress
    , socketPort : Int
    , timeout : Int
    }


decodeConnInfo : D.Decoder ConnectionInfo
decodeConnInfo =
    D.map3 ConnectionInfo
        ( D.field "ip address" decodeIpAddress )

        ( D.field "port" D.int )
        ( D.field "timeout" D.int)


encodeIpPort : Model -> E.Value
encodeIpPort model =
    E.object
        [ ( "ip address", E.string <| unsafeShowIp model.ipAddress)
        , ( "port", E.int model.socketPort )
        , ( "timeout", E.int  model.timeout)
        ]


-- ModData
--------------------------------------------------------------------------------------------------

type alias ModData =
    { modName : String
    , modRegType : RegType
    , modAddress : Int
    , modValue : ModValue
    , modUid : Int
    , modDescription : String
    }

type RegType
    = InputRegister
    | HoldingRegister

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)
getRegType : RegType -> String
getRegType rt =
    case rt of
        InputRegister -> "input register"
        HoldingRegister -> "holding register"

encodeRegister : ModData -> E.Value
encodeRegister md =
    E.object
        [ ( "name" , E.string md.modName)
        , ( "register type" , E.string <| getRegType md.modRegType )
        , ( "address", E.int md.modAddress )
        , ( "register value" , encodeModValue md.modValue )
        , ( "uid", E.int md.modUid )
        , ( "description", E.string md.modDescription )
        ]

encodeModValue : ModValue -> E.Value
encodeModValue mv =
    case mv of
        ModWord (Just x) -> E.object
            [ ( "type", E.string "word" )
            , ( "value", E.int x)
            ]
        ModWord Nothing -> E.object
            [ ( "type", E.string "word" )
            ]
        ModFloat (Just x) -> E.object
            [ ( "type", E.string "float" )
            , ( "value", E.float x)
            ]
        ModFloat Nothing -> E.object
            [ ( "type", E.string "float" )
            ]

decodeModData : D.Decoder ModData
decodeModData =
    D.map6 ModData
        ( D.field "name" D.string )
        ( D.field "register type" decodeRegType )
        ( D.field "address" D.int )
        ( D.field "register value" decodeModValue )
        ( D.field "uid" D.int )
        ( D.field "description" D.string )

decodeModValue : D.Decoder ModValue
decodeModValue =
    D.field "type" D.string |> D.andThen (\s ->
        case s of
            "word" -> D.map ModWord <| D.field "value" (D.nullable D.int)
            "float" -> D.map ModFloat <| D.field "value" (D.nullable D.float)
            _ -> D.fail "Not a valid ModValue"
    )

-- find a way to fail on non valid input
decodeRegType : D.Decoder RegType
decodeRegType =
    D.map (\s ->
        case s of
            "input register" -> InputRegister
            "holding register" -> HoldingRegister
            _ -> InputRegister
    ) D.string

-- ActiveMenu
--------------------------------------------------------------------------------------------------

type ActiveMenu
    = NoneActive
    | ConnectMenu
    | ImportRegisters

getChangedMenu : Model -> ActiveMenu -> ActiveMenu
getChangedMenu model newActive =
    if
        model.activeMenu == newActive
    then
        NoneActive
    else
        newActive