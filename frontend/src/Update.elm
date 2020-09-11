module Update exposing (update, initCmd)

import Http

import Json.Decode as D
import Json.Encode as E
import File
import File.Select as Select
import Task

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
    , encodeIpPort
    , replaceModData
    , ActiveTab(..)
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
                    Just b ->
                        if b < 0 || b > 255
                        then ( model, Cmd.none )
                        else ( { model | ipAddress = setIpAddressByte byte model.ipAddress (Just b) }, Cmd.none )

        ChangePort portNum ->
            if String.isEmpty portNum
            then ( { model | socketPort = Nothing }, Cmd.none )
            else
                case String.toInt portNum of
                    Nothing -> ( model , Cmd.none )
                    Just p ->
                        if p < 0 || p > 65535
                        then ( model , Cmd.none )
                        else ( { model | socketPort = Just p }, Cmd.none )

        ChangeTimeout tm ->
            if String.isEmpty tm
            then ( { model | timeout = Nothing }, Cmd.none )
            else
                case String.toInt tm of
                    Nothing -> ( model , Cmd.none )
                    Just t ->
                        if t < 0 || t > 65535
                        then ( model , Cmd.none )
                        else ( { model | timeout = Just t }, Cmd.none )

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

        ChangeActiveTab tab -> ( { model | activeTab = tab }, Cmd.none )

        CsvRequested -> ( model , Select.file [] CsvSelected )

        CsvSelected file ->
            ( { model | csvFileName = Just <| File.name file } , Task.perform CsvLoaded (File.toString file) )

        CsvLoaded content ->
            ( { model
                    | csvContent = Just content
                    , csvLoaded = False
              }
            , Cmd.none
            )

        ModDataRequest -> ( model, requestModData model )

        ReceivedModData (Err err) ->
            ( { model | status = Bad <| showHttpError err }, Cmd.none )

        ReceivedModData (Ok md) ->
            ( { model
                | modData = md
                , csvLoaded = True
                }
            , Cmd.none
            )

        SelectAllChecked b ->
            case model.activeTab of
                ModDataTable ->
                    (
                    { model |
                        selectAllCheckbox = b
                        , modData = List.map (\md -> { md | selected = b}) model.modData
                    }
                    , Cmd.none
                    )
                _ -> ( { model | selectAllCheckbox = b } , Cmd.none )


        ModDataChecked idx checked ->
            ( { model | modData = List.indexedMap (replaceModData idx checked) model.modData }, Cmd.none)



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

requestModData : Model -> Cmd Msg
requestModData model =
    Http.post
        { url = "http://localhost:4000/parseModData"
        , body = Http.jsonBody <| E.string <| Maybe.withDefault "" model.csvContent
        , expect = Http.expectJson ReceivedModData <| D.list decodeModData
        }





