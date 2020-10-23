module Types exposing
    ( ActiveTab(..)
    , ByteOrder(..)
    , ConnectStatus(..)
    , ConnectionInfo(..)
    , KeepAliveResponse(..)
    , Model
    , Msg(..)
    , SettingsOptions(..)
    , decodeByteOrder
    , decodeConnInfo
    , decodeKeepAliveResponse
    , encodeByteOrder
    , encodeKeepAlive
    , encodeTCPConnectionInfo
    , encodeTCPConnectionRequest
    , showByteOrderResponse
    , showConnInfo
    , showConnectStatus
    , showKeepAliveResponse
    , toByteOrder
    )

import Dropdown exposing (..)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E exposing (Value)
import ModData
    exposing
        ( ModData
        , ModDataUpdate
        , ModValue(..)
        , RegType(..)
        )
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



----------------------------------------------------------------------------------------------------------------------------------
-- Msg
-----------------------------------------------------------------------------------------------------------------------------------


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
    | SetActiveSetting (Setting SettingsOptions Msg)
    | KeepAliveMsg Int Int Bool
    | KeepAliveIdleMsg Int Int String
    | KeepAliveIntervalMsg Int Int String
    | KeepAliveResponseMsg (Result Http.Error KeepAliveResponse)
    | ChangeByteOrderMsg Int Int SettingsOptions
    | ChangeByteOrderResponse (Result Http.Error ByteOrder)
    | RegRegTypeDrop (Option RegType Msg)
    | RegValueTypeDrop (Option ModValue Msg)
    | RegAddress String
    | RegUid String
    | RegToggleRW ReadWrite
    | RegNumber String
    | RegModValue String
    | UpdateRegMdu
    | UpdateRegMduResponse (Result Http.Error (List ModDataUpdate))
    | NoOp



----------------------------------------------------------------------------------------------------------------------------------
-- Model
-----------------------------------------------------------------------------------------------------------------------------------


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
    , settings : List (Setting SettingsOptions Msg)
    , keepAlive : Bool
    , keepAliveIdle : Maybe Int -- in seconds
    , keepAliveInterval : Maybe Int -- in seconds
    , regTypeDd : Dropdown RegType Msg
    , valueTypeDd : Dropdown ModValue Msg
    , regAddress : Maybe Int
    , regUid : Maybe Int
    , regNumReg : Maybe Int
    , regMdu : ModDataUpdate
    , regResponse : List ModDataUpdate
    }



----------------------------------------------------------------------------------------------------------------------------------
-- SettingsOptions
-----------------------------------------------------------------------------------------------------------------------------------


type SettingsOptions
    = SetLE
    | SetBE



----------------------------------------------------------------------------------------------------------------------------------
-- Connect Status
-----------------------------------------------------------------------------------------------------------------------------------


type ConnectStatus
    = Connect
    | Connecting
    | Connected
    | Disconnecting


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



--------------------------------------------------------------------------------------------------
-- ActiveTab
--------------------------------------------------------------------------------------------------


type ActiveTab
    = ConnectMenu
    | ImportMenu
    | RegistersTab
    | ModDataTab
    | HeartbeatTab
    | SettingsTab



--------------------------------------------------------------------------------------------------
-- Keep Alive
--------------------------------------------------------------------------------------------------


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


toByteOrder : SettingsOptions -> ByteOrder
toByteOrder option =
    case option of
        SetLE ->
            LE

        SetBE ->
            BE
