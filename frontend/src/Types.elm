module Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , ModValue (..)
    , ModData
    , getRegType
    , Status (..)
    , showStatus
    , IpAddress
    , showIp
    , IpAddressByte (..)
    , changeIpAddressByte
    , showIpAddressByte
    )

import Http

type Msg
    = ReadRegisters (Result Http.Error (List ModData))
    | RefreshRequest (List ModData)
    | ConnectRequest (IpAddress , Int)
    | ConnectedResponse (Result Http.Error () )
    | ChangeIpAddressByte IpAddressByte

type alias Model =
    { modData : List ModData
    , status : Status
    , ipAddress : IpAddress
    , socketPort : Int
    }

type alias IpAddress =
    { b1 : Int
    , b2 : Int
    , b3 : Int
    , b4 : Int
    }

type IpAddressByte
    = Byte1 Int
    | Byte2 Int
    | Byte3 Int
    | Byte4 Int
    | NoByte

-- See Types/ModData.hs
type RegType
    = InputRegister
    | HoldingRegister

type Status
    = AllGood
    | Loading
    | Bad String
    | Connecting
    | Connected
    | BadIpAddress

getRegType : RegType -> String
getRegType rt =
    case rt of
        InputRegister -> "input register"
        HoldingRegister -> "holding register"

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)

type alias ModData =
    { modName : String
    , modRegType : RegType
    , modAddress : Int
    , modValue : ModValue
    , modUid : Int
    , modDescription : String
    }

showIp : IpAddress -> String
showIp ip =
    String.fromInt ip.b1
    ++ "."
    ++ String.fromInt ip.b2
    ++ "."
    ++ String.fromInt ip.b3
    ++ "."
    ++ String.fromInt ip.b4


showIpAddressByte : IpAddressByte -> String
showIpAddressByte byte =
    case byte of
        Byte1 x -> String.fromInt x
        Byte2 x -> String.fromInt x
        Byte3 x -> String.fromInt x
        Byte4 x -> String.fromInt x
        NoByte -> "No Byte"

changeIpAddressByte : IpAddress -> IpAddressByte -> IpAddress
changeIpAddressByte ip byte =
    case byte of
        Byte1 x -> { ip | b1 = x }
        Byte2 x -> { ip | b2 = x }
        Byte3 x -> { ip | b3 = x }
        Byte4 x -> { ip | b4 = x }
        NoByte -> ip

showStatus : Status -> String
showStatus status =
    case status of
        AllGood -> "all good"
        Loading -> "getting stuff from the server"
        Bad err -> err
        Connecting -> "connecting"
        Connected -> "connected"
        BadIpAddress -> "Invalid ip address"