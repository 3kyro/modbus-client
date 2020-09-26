module Types.IpAddress exposing
    ( IpAddress
    , IpAddressByte(..)
    , changeIpAddressByte
    , decodeIpAddress
    , defaultIpAddr
    , setIpAddressByte
    , showIp
    , showIpAddressByte
    , unsafeShowIp
    )

import Array
import Json.Decode as D


type alias IpAddress =
    { b0 : Maybe Int
    , b1 : Maybe Int
    , b2 : Maybe Int
    , b3 : Maybe Int
    }


type IpAddressByte
    = Byte0
    | Byte1
    | Byte2
    | Byte3


defaultIpAddr : IpAddress
defaultIpAddr =
    IpAddress
        (Just 192)
        (Just 168)
        (Just 0)
        (Just 1)


showIp : IpAddress -> Maybe String
showIp ip =
    Maybe.map4
        unsafeShowFromInt
        ip.b0
        ip.b1
        ip.b2
        ip.b3


unsafeShowIp : IpAddress -> String
unsafeShowIp ip =
    (Maybe.withDefault "" <| Maybe.map String.fromInt ip.b0)
        ++ "."
        ++ (Maybe.withDefault "" <| Maybe.map String.fromInt ip.b1)
        ++ "."
        ++ (Maybe.withDefault "" <| Maybe.map String.fromInt ip.b2)
        ++ "."
        ++ (Maybe.withDefault "" <| Maybe.map String.fromInt ip.b3)


unsafeShowFromInt : Int -> Int -> Int -> Int -> String
unsafeShowFromInt b0 b1 b2 b3 =
    String.fromInt b0
        ++ "."
        ++ String.fromInt b1
        ++ "."
        ++ String.fromInt b2
        ++ "."
        ++ String.fromInt b3


ipFromString : String -> Maybe IpAddress
ipFromString s =
    let
        splits =
            Array.fromList <| String.split "." s

        mip =
            Maybe.map4 getIpAddress
                (Array.get 0 splits)
                (Array.get 1 splits)
                (Array.get 2 splits)
                (Array.get 3 splits)
    in
    mip |> Maybe.andThen (\m -> m)


getIpAddress : String -> String -> String -> String -> Maybe IpAddress
getIpAddress byte1 byte2 byte3 byte4 =
    Maybe.map4 unsafeFromInts
        (byteFromString byte1)
        (byteFromString byte2)
        (byteFromString byte3)
        (byteFromString byte4)


byteFromString : String -> Maybe Int
byteFromString str =
    String.toInt str
        |> Maybe.andThen
            (\i ->
                if i < 0 || i > 255 then
                    Nothing

                else
                    Just i
            )


unsafeFromInts : Int -> Int -> Int -> Int -> IpAddress
unsafeFromInts b0 b1 b2 b3 =
    IpAddress
        (Just b0)
        (Just b1)
        (Just b2)
        (Just b3)


decodeIpAddress : D.Decoder IpAddress
decodeIpAddress =
    D.string
        |> D.andThen
            (\str ->
                case ipFromString str of
                    Nothing ->
                        D.fail "Not an Ip Address"

                    Just ip ->
                        D.succeed ip
            )


showIpAddressByte : IpAddressByte -> IpAddress -> String
showIpAddressByte byte ip =
    case byte of
        Byte0 ->
            Maybe.withDefault "" <| Maybe.map String.fromInt ip.b0

        Byte1 ->
            Maybe.withDefault "" <| Maybe.map String.fromInt ip.b1

        Byte2 ->
            Maybe.withDefault "" <| Maybe.map String.fromInt ip.b2

        Byte3 ->
            Maybe.withDefault "" <| Maybe.map String.fromInt ip.b3


changeIpAddressByte : IpAddressByte -> IpAddress -> String -> Maybe IpAddress
changeIpAddressByte byte ip str =
    let
        str1 =
            showIpAddressByte Byte0 ip

        str2 =
            showIpAddressByte Byte1 ip

        str3 =
            showIpAddressByte Byte2 ip

        str4 =
            showIpAddressByte Byte3 ip
    in
    String.toInt str
        |> Maybe.andThen
            (\_ ->
                case byte of
                    Byte0 ->
                        getIpAddress str str2 str3 str4

                    Byte1 ->
                        getIpAddress str1 str str3 str4

                    Byte2 ->
                        getIpAddress str1 str2 str str4

                    Byte3 ->
                        getIpAddress str1 str2 str3 str
            )


setIpAddressByte : IpAddressByte -> IpAddress -> Maybe Int -> IpAddress
setIpAddressByte byte ip mint =
    case byte of
        Byte0 ->
            { ip | b0 = mint }

        Byte1 ->
            { ip | b1 = mint }

        Byte2 ->
            { ip | b2 = mint }

        Byte3 ->
            { ip | b3 = mint }
