module Update exposing (update)

import Http

import Json.Decode as D
import Json.Encode as E

import Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , getRegType
    , ModValue (..)
    , Status (..)
    , ModData
    , IpAddressByte (..)
    , showIp
    , changeIpAddressByte
    , ConnectStatus(..)
    )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        ReadRegisters (Ok regs) ->
            ( { model | modData = regs , status = AllGood } , Cmd.none )
        ReadRegisters (Err err) ->
            ( { model | status = Bad <| showHttpError err }, Cmd.none )
        RefreshRequest regs -> ( { model | status = Loading } , refreshRequest regs )
        ConnectRequest -> ( { model | connectStatus = Connecting } , connectRequest model )
        ConnectedResponse (Ok _) ->
            ( { model | connectStatus = Connected } , Cmd.none )
        ConnectedResponse(Err err) ->
            ( { model |
                  status = Bad <| showHttpError err
                , connectStatus = Connect }
            , Cmd.none
            )
        ChangeIpAddressByte (NoByte) ->
            ( { model | status = BadIpAddress }, Cmd.none )
        ChangeIpAddressByte byte ->
            ( { model | ipAddress = changeIpAddressByte model.ipAddress byte }, Cmd.none )
        ChangePort portNum ->
            case String.toInt portNum of
                Nothing -> ( { model | status = BadPort }, Cmd.none )
                Just p -> ( { model | socketPort = p }, Cmd.none )
        ChangeTimeout tm ->
            case String.toInt tm of
                Nothing -> ( { model | status = BadTimeout }, Cmd.none )
                Just t -> ( { model | timeout = t }, Cmd.none )


connectRequest : Model -> Cmd Msg
connectRequest model =
    Http.post
        { url = "http://localhost:4000/connect"
        , body = Http.jsonBody <| encodeIpPort model
        , expect = Http.expectWhatever ConnectedResponse
        }

encodeIpPort : Model -> E.Value
encodeIpPort model =
    E.object
        [ ( "ip address", E.string <| showIp model.ipAddress)
        , ( "port", E.int model.socketPort )
        , ( "timeout", E.int  model.timeout)
        ]

showHttpError : Http.Error -> String
showHttpError err =
    case err of
       Http.BadUrl s -> s
       Http.Timeout -> "timeout"
       Http.NetworkError -> "Network Error"
       Http.BadStatus s -> "Bad status " ++ String.fromInt s
       Http.BadBody s -> s

refreshRequest : List ModData -> Cmd Msg
refreshRequest regs =
    Http.post
        { url = "http://localhost:4000/register"
        , body = Http.jsonBody <| E.list encodeRegister regs
        , expect = Http.expectJson ReadRegisters <| D.list decodeModData
        }

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