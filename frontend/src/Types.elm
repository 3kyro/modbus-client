module Types exposing
    ( ActiveTab(..)
    , ConnectStatus(..)
    , ConnectionInfo (..)
    , MFloat
    , ModData
    , ModDataUpdate
    , ModValue(..)
    , Model
    , Msg(..)
    , ReadWrite(..)
    , RegType(..)
    , decodeConnInfo
    , decodeModData
    , decodeModDataUpdate
    , encodeTCPConnectionInfo
    , encodeModData
    , encodeModDataUpdate
    , flipRW
    , fromFloat
    , fromModType
    , getModValue
    , getModValueType
    , getRegType
    , newModDataUpdate
    , replaceModDataSelected
    , replaceModDataWrite
    , showConnInfo
    , showConnectStatus
    , toMFloat
    , writeableReg
    , keepAliveJson
    )

import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Notifications
    exposing
        ( Notification
        , StatusBarState(..)
        )
import String exposing (fromFloat)
import Time
import Types.IpAddress
    exposing
        ( IpAddress
        , IpAddressByte
        , decodeIpAddress
        , showIp
        , unsafeShowIp
        )
import Settings exposing (Setting)

type Msg
    = ReadRegisters (Result Http.Error (List ModDataUpdate))
    | ReceivedConnectionInfo (Result Http.Error (Maybe ConnectionInfo))
    | RefreshRequest (List ModDataUpdate)
    | ConnectRequest
    | ConnectedResponse (Result Http.Error ())
    | ChangeIpAddress IpAddressByte String
    | ChangePort String
    | ChangeTimeout String
    | DisconnectRequest
    | DisconnectedResponse (Result Http.Error ())
    | ChangeActiveTab ActiveTab
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | ModDataRequest
    | ReceivedModData (Result Http.Error (List ModData))
    | SelectAllChecked Bool
    | ModDataChecked Int Bool
    | ToggleWriteAll ReadWrite
    | ModDataWrite Int ReadWrite
    | ChangeModDataValue Int String
    | ExpandStatus
    | TimeZone Time.Zone
    | InitTime Time.Posix
    | NewTime Time.Posix
    | ExpandNotification Notification
    | SetActiveSetting (Setting Msg)
    | KeepAliveMsg Int Int Bool
    | KeepAliveIntervalMsg Int Int String
    | NoOp


type alias Model =
    { modDataUpdate : List ModDataUpdate
    , statusBarState : StatusBarState
    , notifications : List Notification
    , connectStatus : ConnectStatus
    , ipAddress : IpAddress
    , socketPort : Maybe Int
    , serialPort : Maybe String
    , timeout : Maybe Int
    , activeTab : ActiveTab
    , csvFileName : Maybe String
    , csvContent : Maybe String
    , csvLoaded : Bool
    , selectAllCheckbox : Bool
    , selectSome : Bool
    , readWriteAll : ReadWrite
    , timePosix : Time.Posix
    , timeZone : Time.Zone
    , settings : List (Setting Msg)
    , keepAlive : Bool
    , keepAliveInterval : Maybe Int
    }


type ConnectStatus
    = Connect
    | Connecting
    | Connected
    | Disconnecting


newModDataUpdate : List ModData -> List ModDataUpdate
newModDataUpdate mds =
    List.map (\md -> ModDataUpdate md False Read) mds


showConnectStatus : ConnectStatus -> String
showConnectStatus st =
    case st of
        Connect ->
            "Connect"

        Connecting ->
            "Connecting"

        Connected ->
            "Connected"

        Disconnecting ->
            "Disconnecting"




----------------------------------------------------------------------------------------------------------------------------------
-- Connection Info
-----------------------------------------------------------------------------------------------------------------------------------


type ConnectionInfo
    = TCPConnectionInfo
        { ipAddress : IpAddress
        , socketPort : Int
        , timeout : Int
        }
    | RTUConnectionInfo
        { rtuAddress : String
        , timeout : Int
        }


decodeConnInfo : D.Decoder ConnectionInfo
decodeConnInfo =
    D.field "connection type" D.string
        |> D.andThen
            (\s ->
                case s of
                    "tcp" ->
                        D.map3 getTCPConnectionInfo
                            (D.field "ip address" decodeIpAddress)
                            (D.field "port" D.int)
                            (D.field "timeout" D.int)

                    "rtu" ->
                        D.map2 getRTUConnectionInfo
                            (D.field "serial port" D.string)
                            (D.field "timeout" D.int)

                    _ ->
                        D.fail "Not a connection info"
            )


encodeTCPConnectionInfo : Model -> E.Value
encodeTCPConnectionInfo model =
    E.object
        [ ( "connection type" , E.string "tcp")
        , ( "ip address", E.string <| unsafeShowIp model.ipAddress )
        , ( "port", E.int <| Maybe.withDefault 0 model.socketPort )
        , ( "timeout", E.int <| Maybe.withDefault 0 model.timeout )
        ]

getTCPConnectionInfo : IpAddress -> Int -> Int -> ConnectionInfo
getTCPConnectionInfo ip portNum tm =
    TCPConnectionInfo
        { ipAddress = ip
        , socketPort = portNum
        , timeout = tm
        }


getRTUConnectionInfo : String -> Int -> ConnectionInfo
getRTUConnectionInfo address tm =
    RTUConnectionInfo
        { rtuAddress = address
        , timeout = tm
        }




showConnInfo : ConnectionInfo -> String
showConnInfo connInfo =
    case connInfo of
        TCPConnectionInfo conn ->
            ("IP Address: " ++ Maybe.withDefault "N/A" (showIp conn.ipAddress) ++ "\n")
                ++ ("Port: " ++ String.fromInt conn.socketPort ++ "\n")
                ++ ("Timeout: " ++ String.fromInt conn.timeout)

        RTUConnectionInfo conn ->
            ("Serial Port: " ++ conn.rtuAddress ++ "\n")
                ++ ("Timeout: " ++ String.fromInt conn.timeout)



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
        InputRegister ->
            False

        HoldingRegister ->
            True


type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe MFloat)


getRegType : RegType -> String
getRegType rt =
    case rt of
        InputRegister ->
            "input register"

        HoldingRegister ->
            "holding register"


getModValueType : ModValue -> String
getModValueType mv =
    case mv of
        ModWord _ ->
            "Word"

        ModFloat _ ->
            "Float"


getModValue : ModValue -> Maybe String
getModValue mv =
    case mv of
        ModWord v ->
            Maybe.map String.fromInt v

        ModFloat v ->
            Maybe.map showMFloat v


encodeModData : ModData -> E.Value
encodeModData md =
    E.object
        [ ( "name", E.string md.modName )
        , ( "register type", E.string <| getRegType md.modRegType )
        , ( "address", E.int md.modAddress )
        , ( "register value", encodeModValue md.modValue )
        , ( "uid", E.int md.modUid )
        , ( "description", E.string md.modDescription )
        ]


encodeModDataUpdate : ModDataUpdate -> E.Value
encodeModDataUpdate mdu =
    E.object
        [ ( "modData", encodeModData mdu.mduModData )
        , ( "selected", E.bool <| mdu.mduSelected )
        , ( "rw", encodeRW mdu.mduRW )
        ]


decodeModDataUpdate : D.Decoder ModDataUpdate
decodeModDataUpdate =
    D.map3 ModDataUpdate
        (D.field "modData" decodeModData)
        (D.field "selected" D.bool)
        (D.field "rw" decodeRW)


encodeModValue : ModValue -> E.Value
encodeModValue mv =
    case mv of
        ModWord (Just x) ->
            E.object
                [ ( "type", E.string "word" )
                , ( "value", E.int x )
                ]

        ModWord Nothing ->
            E.object
                [ ( "type", E.string "word" )
                ]

        ModFloat (Just x) ->
            E.object
                [ ( "type", E.string "float" )
                , ( "value", E.float x.flt )
                ]

        ModFloat Nothing ->
            E.object
                [ ( "type", E.string "float" )
                ]


decodeModData : D.Decoder ModData
decodeModData =
    D.map6 ModData
        (D.field "name" D.string)
        (D.field "register type" decodeRegType)
        (D.field "address" D.int)
        (D.field "register value" decodeModValue)
        (D.field "uid" D.int)
        (D.field "description" D.string)


decodeModValue : D.Decoder ModValue
decodeModValue =
    D.field "type" D.string
        |> D.andThen
            (\s ->
                case s of
                    "word" ->
                        D.map ModWord <| D.field "value" (D.nullable D.int)

                    "float" ->
                        D.map ModFloat <| D.field "value" (D.nullable decodeMFloat)

                    _ ->
                        D.fail "Not a valid ModValue"
            )


decodeRegType : D.Decoder RegType
decodeRegType =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "input register" ->
                        D.succeed InputRegister

                    "holding register" ->
                        D.succeed HoldingRegister

                    _ ->
                        D.fail "Not a Register Type"
            )


decodeMFloat : D.Decoder MFloat
decodeMFloat =
    D.map fromFloat D.float


replaceModDataSelected : Int -> Bool -> Int -> ModDataUpdate -> ModDataUpdate
replaceModDataSelected idx checked =
    \i md ->
        if i == idx then
            { md | mduSelected = checked }

        else
            md


replaceModDataWrite : Int -> ReadWrite -> Int -> ModDataUpdate -> ModDataUpdate
replaceModDataWrite idx rw =
    \i md ->
        if i == idx && writeableReg md.mduModData then
            { md | mduRW = rw }

        else
            md


type ReadWrite
    = Read
    | Write


flipRW : ReadWrite -> ReadWrite
flipRW rw =
    case rw of
        Read ->
            Write

        Write ->
            Read


encodeRW : ReadWrite -> E.Value
encodeRW rw =
    case rw of
        Read ->
            E.string "read"

        Write ->
            E.string "write"


decodeRW : D.Decoder ReadWrite
decodeRW =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "read" ->
                        D.succeed Read

                    "write" ->
                        D.succeed Write

                    _ ->
                        D.fail "Neither Read or Write"
            )


fromModType : ModData -> String -> ModData
fromModType md str =
    case md.modValue of
        ModWord _ ->
            { md | modValue = ModWord <| String.toInt str }

        ModFloat _ ->
            { md | modValue = ModFloat <| toMFloat str }



--------------------------------------------------------------------------------------------------
-- ActiveTab


type ActiveTab
    = ConnectMenu
    | ImportMenu
    | InputRegistersTab
    | HoldingRegistersTab
    | ModDataTab
    | HeartbeatTab
    | SettingsTab



-- Custom type to overcome a limitaion of elm when updating float inputs
-- Speciffically "1." is a valid float that is shown as "1"
-- This blocks inputs after a dot is typed


type alias MFloat =
    { str : String
    , flt : Float
    }



-- always show the string, not the float


showMFloat : MFloat -> String
showMFloat mf =
    mf.str



-- save the string in case of a valid parse


toMFloat : String -> Maybe MFloat
toMFloat s =
    Maybe.map (MFloat s) <| String.toFloat s


fromFloat : Float -> MFloat
fromFloat f =
    MFloat (String.fromFloat f) f

--------------------------------------------------------------------------------------------------
-- Keep Alive

keepAliveJson : Model -> E.Value
keepAliveJson model =
    E.object
        [ ( "keep alive", E.bool model.keepAlive )
        , ( "interval", E.int <| Maybe.withDefault 1 model.keepAliveInterval )
        ]
