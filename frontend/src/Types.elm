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
    , ActiveTab (..)
    , replaceModDataWrite
    , writeableReg
    , replaceModDataSelected
    , getModValue
    , getModValueType
    , getModSelected
    , encodeIpPort
    , ReadWrite (..)
    , flipRW
    )


import Http
import Json.Decode as D
import Json.Encode as E
import File exposing (File)

import Types.IpAddress exposing
    ( IpAddress
    , decodeIpAddress
    , IpAddressByte
    , unsafeShowIp
    )

type Msg
    = ReadRegisters (Result Http.Error (List ModData))
    | ReceivedConnectionInfo ( Result Http.Error (Maybe ConnectionInfo))
    | RefreshRequest (List ModData)
    | ConnectRequest
    | ConnectedResponse (Result Http.Error () )
    | ChangeIpAddress IpAddressByte String
    | ChangePort String
    | ChangeTimeout String
    | DisconnectRequest
    | DisconnectedResponse (Result Http.Error () )
    | ChangeActiveTab ActiveTab
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | ModDataRequest
    | ReceivedModData ( Result Http.Error (List ModData))
    | SelectAllChecked Bool
    | ModDataChecked Int Bool
    | ToggleWriteAll ReadWrite
    | ModDataWrite Int ReadWrite
    | ChangeModDataValue Int String

type alias Model =
    { modData : List ModData
    , status : Status
    , connectStatus : ConnectStatus
    , ipAddress : IpAddress
    , socketPort : Maybe Int
    , timeout : Maybe Int
    , activeTab : ActiveTab
    , csvFileName : Maybe String
    , csvContent : Maybe String
    , csvLoaded : Bool
    , selectAllCheckbox : Bool
    , selectSome : Bool
    , readWriteAll : ReadWrite
    }
type ConnectStatus
    = Connect
    | Connecting
    | Connected
    | Disconnecting

showConnectStatus : ConnectStatus -> String
showConnectStatus st =
    case st of
        Connect -> "Connect"
        Connecting -> "Connecting"
        Connected -> "Connected"
        Disconnecting -> "Disconnecting"

type Status
    = AllGood
    | Loading
    | Bad String

showStatus : Status -> String
showStatus status =
    case status of
        AllGood -> "all good"
        Loading -> "getting stuff from the server"
        Bad err -> err

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
        , ( "port", E.int <| Maybe.withDefault 0 model.socketPort )
        , ( "timeout", E.int <| Maybe.withDefault 0  model.timeout)
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
    , selected : Bool
    , rw : ReadWrite
    }

type RegType
    = InputRegister
    | HoldingRegister

writeableReg : ModData -> Bool
writeableReg md =
    case md.modRegType of
        InputRegister -> False
        HoldingRegister -> True

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)
getRegType : RegType -> String
getRegType rt =
    case rt of
        InputRegister -> "input register"
        HoldingRegister -> "holding register"

getModValueType : ModValue -> String
getModValueType mv =
    case mv of
        ModWord _ -> "Word"
        ModFloat _ -> "Float"

getModValue : ModValue -> Maybe String
getModValue mv =
    case mv of
       ModWord v -> Maybe.map String.fromInt v
       ModFloat v -> Maybe.map String.fromFloat v

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
    D.map8 ModData
        ( D.field "name" D.string )
        ( D.field "register type" decodeRegType )
        ( D.field "address" D.int )
        ( D.field "register value" decodeModValue )
        ( D.field "uid" D.int )
        ( D.field "description" D.string )
        ( D.succeed False )
        ( D.succeed Read )


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

replaceModDataSelected : Int -> Bool -> Int -> ModData -> ModData
replaceModDataSelected idx checked =
    \i md ->
        if i == idx
        then { md | selected = checked }
        else md

replaceModDataWrite : Int -> ReadWrite -> Int -> ModData -> ModData
replaceModDataWrite idx rw =
    \i md ->
        if i == idx && writeableReg md
        then { md | rw = rw }
        else md


getModSelected : ModData -> Bool
getModSelected md = md.selected

type ReadWrite
    = Read
    | Write

flipRW : ReadWrite -> ReadWrite
flipRW rw =
    case rw of
        Read -> Write
        Write -> Read



-- ActiveTab
--------------------------------------------------------------------------------------------------

type ActiveTab
    = ConnectMenu
    | ImportMenu
    | InputRegistersTable
    | HoldingRegistersTable
    | ModDataTable
    | HeartbeatTable