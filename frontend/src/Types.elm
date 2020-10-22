module Types exposing
    ( ActiveTab(..)
    , ByteOrder(..)
    , ConnectStatus(..)
    , ConnectionInfo(..)
    , KeepAliveResponse(..)
    , MFloat
    , ModData
    , ModDataUpdate
    , ModValue(..)
    , Model
    , Msg(..)
    , RegType(..)
    , SettingOption(..)
    , ValueType(..)
    , decodeByteOrder
    , decodeConnInfo
    , decodeKeepAliveResponse
    , decodeModData
    , decodeModDataUpdate
    , encodeByteOrder
    , encodeKeepAlive
    , encodeModData
    , encodeModDataUpdate
    , encodeTCPConnectionInfo
    , encodeTCPConnectionRequest
    , fromFloat
    , fromModType
    , fromModTypeUpdate
    , getModValue
    , getModValueType
    , getModValueUpdate
    , getRegType
    , isWriteableReg
    , newModDataUpdate
    , replaceModDataSelected
    , replaceModDataWrite
    , setRegAddressUpdate
    , setRegRWUpdate
    , setRegTypeUpdate
    , setRegUidUpdate
    , showByteOrderResponse
    , showConnInfo
    , showConnectStatus
    , showKeepAliveResponse
    , toByteOrder
    , toMFloat
    )

import Dropdown exposing (..)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import Notifications
    exposing
        ( Notification
        , StatusBarState(..)
        )
import ReadWrite
    exposing
        ( ReadWrite(..)
        , decodeRW
        , encodeRW
        )
import Settings exposing (Setting)
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
    | SetActiveSetting (Setting SettingOption Msg)
    | KeepAliveMsg Int Int Bool
    | KeepAliveIdleMsg Int Int String
    | KeepAliveIntervalMsg Int Int String
    | KeepAliveResponseMsg (Result Http.Error KeepAliveResponse)
    | ChangeByteOrderMsg Int Int SettingOption
    | ChangeByteOrderResponse (Result Http.Error ByteOrder)
    | RegRegTypeDrop (Option RegType Msg)
    | RegValueTypeDrop (Option ValueType Msg)
    | RegAddress String
    | RegUid String
    | RegToggleRW ReadWrite
    | RegNumber String
    | RegModValue String
    | NoOp


type alias Model =
    { modDataUpdate : List ModDataUpdate
    , statusBarState : StatusBarState
    , notifications : List Notification
    , connectStatus : ConnectStatus
    , ipAddress : IpAddress
    , socketPort : Maybe Int
    , serialPort : Maybe String
    , timeout : Maybe Int -- in seconds
    , byteOrder : ByteOrder
    , activeTab : ActiveTab
    , csvFileName : Maybe String
    , csvContent : Maybe String
    , csvLoaded : Bool
    , selectAllCheckbox : Bool
    , selectSome : Bool
    , readWriteAll : ReadWrite
    , timePosix : Time.Posix
    , timeZone : Time.Zone
    , settings : List (Setting SettingOption Msg)
    , keepAlive : Bool
    , keepAliveIdle : Maybe Int -- in seconds
    , keepAliveInterval : Maybe Int -- in seconds
    , regTypeDd : Dropdown RegType Msg
    , valueTypeDd : Dropdown ValueType Msg
    , regAddress : Maybe Int
    , regUid : Maybe Int
    , regNumReg : Maybe Int
    , regMdu : ModDataUpdate
    , regResponse : List ModDataUpdate
    }


type ConnectStatus
    = Connect
    | Connecting
    | Connected
    | Disconnecting


type SettingOption
    = SetLE
    | SetBE


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
        , timeout : Int -- in seconds
        }
    | RTUConnectionInfo
        { rtuAddress : String
        , timeout : Int -- in seconds
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


encodeTCPConnectionRequest : Model -> E.Value
encodeTCPConnectionRequest model =
    E.object
        [ ( "connection info", encodeTCPConnectionInfo model )
        , ( "keep alive", encodeKeepAlive model model.keepAlive )
        ]


encodeTCPConnectionInfo : Model -> E.Value
encodeTCPConnectionInfo model =
    E.object
        [ ( "connection type", E.string "tcp" )
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


isWriteableReg : RegType -> Bool
isWriteableReg rt =
    case rt of
        InputRegister ->
            False

        HoldingRegister ->
            True


type ValueType
    = VWord
    | VFloat


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


getModValueUpdate : ModDataUpdate -> Maybe String
getModValueUpdate mdu =
    getModValue mdu.mduModData.modValue


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
        if i == idx && isWriteableReg md.mduModData.modRegType then
            { md | mduRW = rw }

        else
            md


fromModType : ModData -> String -> ModData
fromModType md str =
    case md.modValue of
        ModWord _ ->
            { md | modValue = ModWord <| String.toInt str }

        ModFloat _ ->
            { md | modValue = ModFloat <| toMFloat str }


fromModTypeUpdate : ModDataUpdate -> String -> ModDataUpdate
fromModTypeUpdate mdu str =
    { mdu | mduModData = fromModType mdu.mduModData str }


setRegType : ModData -> RegType -> ModData
setRegType md rt =
    { md | modRegType = rt }


setRegTypeUpdate : ModDataUpdate -> RegType -> ModDataUpdate
setRegTypeUpdate mdu rt =
    if isWriteableReg rt then
        { mdu | mduModData = setRegType mdu.mduModData rt }

    else
        -- Only writeable register can be Write
        { mdu
            | mduModData = setRegType mdu.mduModData rt
            , mduRW = Read
        }


setRegAddress : ModData -> Int -> ModData
setRegAddress md addr =
    { md | modAddress = addr }


setRegAddressUpdate : ModDataUpdate -> Int -> ModDataUpdate
setRegAddressUpdate mdu addr =
    { mdu | mduModData = setRegAddress mdu.mduModData addr }


setRegUid : ModData -> Int -> ModData
setRegUid md uid =
    { md | modUid = uid }


setRegUidUpdate : ModDataUpdate -> Int -> ModDataUpdate
setRegUidUpdate mdu uid =
    { mdu | mduModData = setRegUid mdu.mduModData uid }


setRegRWUpdate : ModDataUpdate -> ReadWrite -> ModDataUpdate
setRegRWUpdate mdu rw =
    { mdu | mduRW = rw }



--------------------------------------------------------------------------------------------------
-- ActiveTab


type ActiveTab
    = ConnectMenu
    | ImportMenu
    | RegistersTab
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


encodeKeepAlive : Model -> Bool -> E.Value
encodeKeepAlive model flag =
    E.object
        [ ( "flag", E.bool flag )
        , ( "idle", E.int <| Maybe.withDefault 60 model.keepAliveIdle )
        , ( "interval", E.int <| Maybe.withDefault 10 model.keepAliveInterval )
        ]


type KeepAliveResponse
    = KeepAliveActivated
    | KeepAliveDisactivated


showKeepAliveResponse : KeepAliveResponse -> String
showKeepAliveResponse kar =
    case kar of
        KeepAliveActivated ->
            "Keep alive activated"

        KeepAliveDisactivated ->
            "Keep alive disactivated"


decodeKeepAliveResponse : D.Decoder KeepAliveResponse
decodeKeepAliveResponse =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Keep alive activated" ->
                        D.succeed KeepAliveActivated

                    "Keep alive disactivated" ->
                        D.succeed KeepAliveDisactivated

                    _ ->
                        D.fail "Not a KeepAliveResponse"
            )



--------------------------------------------------------------------------------------------------
-- ByteOrder
--------------------------------------------------------------------------------------------------


type ByteOrder
    = LE
    | BE


encodeByteOrder : ByteOrder -> E.Value
encodeByteOrder order =
    case order of
        LE ->
            E.string "le"

        BE ->
            E.string "be"


decodeByteOrder : D.Decoder ByteOrder
decodeByteOrder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "le" ->
                        D.succeed LE

                    "be" ->
                        D.succeed BE

                    _ ->
                        D.fail "Not a ByteOrder"
            )


showByteOrderResponse : ByteOrder -> String
showByteOrderResponse order =
    case order of
        LE ->
            "Byte order changed to Little Endian"

        BE ->
            "Byte order changed to Big Endian"


toByteOrder : SettingOption -> ByteOrder
toByteOrder option =
    case option of
        SetLE ->
            LE

        SetBE ->
            BE
