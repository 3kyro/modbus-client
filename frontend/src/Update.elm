module Update exposing (update, initCmd)

import Http

import Json.Decode as D
import Json.Encode as E

import Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , ModValue (..)
    , Status (..)
    , ModData
    , ConnectStatus(..)
    , decodeConnInfo
    , encodeRegister
    , decodeModData
    , getChangedMenu
    , encodeIpPort
    )

import Types.IpAddress exposing (setIpAddressByte)
update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        ReadRegisters (Ok regs) ->
            ( { model | modData = regs , status = AllGood } , Cmd.none )

        ReadRegisters (Err err) ->
            ( { model | status = Bad <| showHttpError err }, Cmd.none )

        ReceivedConnectionInfo (Ok (Just conn)) ->
            ( { model
                | ipAddress = conn.ipAddress
                , socketPort = Just conn.socketPort
                , timeout = Just conn.timeout
                , connectStatus = Connected
                }
            , Cmd.none
            )

        ReceivedConnectionInfo (Ok Nothing) -> (model, Cmd.none)

        ReceivedConnectionInfo (Err err) ->
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

        ChangeIpAddress byte str ->
            if String.isEmpty str
            then ( { model | ipAddress = setIpAddressByte byte model.ipAddress Nothing }, Cmd.none )
            else
                case String.toInt str of
                    Nothing -> ( model , Cmd.none )
                    Just b -> ( { model | ipAddress = setIpAddressByte byte model.ipAddress (Just b) }, Cmd.none )

        ChangePort portNum ->
            if String.isEmpty portNum
            then ( { model | socketPort = Nothing }, Cmd.none )
            else
                case String.toInt portNum of
                    Nothing -> ( { model | status = BadPort }, Cmd.none )
                    Just p -> ( { model | socketPort = Just p }, Cmd.none )

        ChangeTimeout tm ->
            if String.isEmpty tm
            then ( { model | timeout = Nothing }, Cmd.none )
            else
                case String.toInt tm of
                    Nothing -> ( { model | status = BadTimeout }, Cmd.none )
                    Just t -> ( { model | timeout = Just t }, Cmd.none )

        DisconnectRequest ->
            case model.connectStatus of
                Connected -> ( { model | connectStatus = Disconnecting } , disconnectRequest )
                _ -> (model, Cmd.none)

        DisconnectedResponse (Ok _) ->
            ( { model | connectStatus = Connect } , Cmd.none )

        DisconnectedResponse(Err err) ->
            ( { model | status = Bad <| showHttpError err
                , connectStatus = Connect }
            , Cmd.none
            )

        ChangeActiveMenu menu -> ( { model | activeMenu = getChangedMenu model menu }, Cmd.none )

initCmd : Cmd Msg
initCmd = connectionInfoRequest
connectionInfoRequest : Cmd Msg
connectionInfoRequest  =
    Http.get
        { url = "http://localhost:4000/connectInfo"
        , expect = Http.expectJson ReceivedConnectionInfo (D.maybe decodeConnInfo)
        }
connectRequest : Model -> Cmd Msg
connectRequest model =
    Http.post
        { url = "http://localhost:4000/connect"
        , body = Http.jsonBody <| encodeIpPort model
        , expect = Http.expectWhatever ConnectedResponse
        }

disconnectRequest : Cmd Msg
disconnectRequest =
    Http.post
        { url = "http://localhost:4000/disconnect"
        , body = Http.jsonBody <| E.string "disconnect"
        , expect = Http.expectWhatever DisconnectedResponse
        }


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







