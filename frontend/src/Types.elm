module Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , ModValue (..)
    , ModData
    , ModDataUpdate
    , getRegType
    , Status (..)
    , StatusBarState (..)
    , showStatus
    , ConnectStatus (..)
    , showConnectStatus
    , ConnectionInfo
    , decodeConnInfo
    , encodeModData
    , decodeModData
    , ActiveTab (..)
    , replaceModDataWrite
    , writeableReg
    , replaceModDataSelected
    , getModValue
    , getModValueType
    , encodeIpPort
    , ReadWrite (..), newModDataUpdate, encodeModDataUpdate
    , flipRW
    , decodeModDataUpdate
    , MFloat
    , toMFloat
    , fromFloat
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
import String exposing (fromFloat)

type Msg
    = ReadRegisters (Result Http.Error (List ModDataUpdate))
    | ReceivedConnectionInfo ( Result Http.Error (Maybe ConnectionInfo))
    | RefreshRequest (List ModDataUpdate)
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
    | ExpandStatus

type alias Model =
    { modDataUpdate : List ModDataUpdate
    , status : Status
    , statusBarState : StatusBarState
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

newModDataUpdate : List ModData -> List ModDataUpdate
newModDataUpdate mds =
    List.map (\md -> ModDataUpdate md False Read )  mds

showConnectStatus : ConnectStatus -> String
showConnectStatus st =
    case st of
        Connect -> "Connect"
        Connecting -> "Connecting"
        Connected -> "Connected"
        Disconnecting -> "Disconnecting"

----------------------------------------------------------------------------------------------------------------------------------
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

type StatusBarState
    = Expanded
    | Retracted


-----------------------------------------------------------------------------------------------------------------------------------

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
    }

type alias ModDataUpdate =
    { mduModData : ModData
    , mduSelected : Bool
    , mduRW : ReadWrite
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
    | ModFloat (Maybe MFloat)
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
       ModFloat v -> Maybe.map showMFloat v

-- ss : String -> Maybe Float
-- ss  s =
--     if String.endsWith "."
--     then
--          case String.fromFloat s of
--             Nothing -> Nothing
--             Just _ -> s

encodeModData : ModData -> E.Value
encodeModData md =
    E.object
        [ ( "name" , E.string md.modName)
        , ( "register type" , E.string <| getRegType md.modRegType )
        , ( "address", E.int md.modAddress )
        , ( "register value" , encodeModValue md.modValue )
        , ( "uid", E.int md.modUid )
        , ( "description", E.string md.modDescription )
        ]

encodeModDataUpdate : ModDataUpdate -> E.Value
encodeModDataUpdate mdu =
    E.object
        [ ( "modData", encodeModData mdu.mduModData)
        , ( "selected" , E.bool <| mdu.mduSelected )
        , ( "rw", encodeRW mdu.mduRW )
        ]

decodeModDataUpdate : D.Decoder ModDataUpdate
decodeModDataUpdate =
    D.map3 ModDataUpdate
        ( D.field "modData" decodeModData )
        ( D.field "selected" D.bool )
        ( D.field "rw" decodeRW )

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
            , ( "value", E.float x.flt)
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
            "float" -> D.map ModFloat <| D.field "value" (D.nullable decodeMFloat)
            _ -> D.fail "Not a valid ModValue"
    )

decodeRegType : D.Decoder RegType
decodeRegType =
    D.string
    |> D.andThen
        (\s ->
            case s of
                "input register" -> D.succeed InputRegister
                "holding register" -> D.succeed HoldingRegister
                _ -> D.fail "Not a Register Type"
        )

decodeMFloat : D.Decoder MFloat
decodeMFloat =
    D.map fromFloat D.float

replaceModDataSelected : Int -> Bool -> Int -> ModDataUpdate -> ModDataUpdate
replaceModDataSelected idx checked =
    \i md ->
        if i == idx
        then { md | mduSelected = checked }
        else md

replaceModDataWrite : Int -> ReadWrite -> Int -> ModDataUpdate -> ModDataUpdate
replaceModDataWrite idx rw =
    \i md ->
        if i == idx && writeableReg md.mduModData

        then { md | mduRW = rw }
        else md

type ReadWrite
    = Read
    | Write

flipRW : ReadWrite -> ReadWrite
flipRW rw =
    case rw of
        Read -> Write
        Write -> Read

encodeRW : ReadWrite -> E.Value
encodeRW rw =
    case rw of
        Read -> E.string "read"
        Write -> E.string "write"

decodeRW : D.Decoder ReadWrite
decodeRW =
    D.string
    |> D.andThen
        (\s ->
            case s of
                "read" -> D.succeed Read
                "write" -> D.succeed Write
                _ -> D.fail "Neither Read or Write"
        )

-- ActiveTab
--------------------------------------------------------------------------------------------------

type ActiveTab
    = ConnectMenu
    | ImportMenu
    | InputRegistersTable
    | HoldingRegistersTable
    | ModDataTable
    | HeartbeatTable


-- Custom type to overcome a limitaion of elm when updating float inputs
-- Speciffically "1." is a valid float that is shown as "1"
-- This blocks inputs after a dot is typed
type alias MFloat =
    { str : String
    , flt : Float
    }

-- always show the string, not the float
showMFloat : MFloat -> String
showMFloat mf = mf.str

-- save the string in case of a valid parse
toMFloat : String -> Maybe MFloat
toMFloat s = Maybe.map (MFloat s) <| String.toFloat s

fromFloat : Float -> MFloat
fromFloat f = MFloat (String.fromFloat f) f
