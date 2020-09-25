module Update exposing (getTimeZone, initCmd, update)

import Array
import File
import File.Select as Select
import Http
import Json.Decode as D
import Json.Encode as E
import Task
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , ModData
        , ModDataUpdate
        , ModValue(..)
        , Model
        , Msg(..)
        , RegType(..)
        , Status(..)
        , StatusBarState(..)
        , decodeConnInfo
        , decodeModData
        , decodeModDataUpdate
        , encodeIpPort
        , encodeModDataUpdate
        , newModDataUpdate
        , replaceModDataSelected
        , replaceModDataWrite
        , toMFloat
        , writeableReg
        )
import Types.IpAddress exposing (setIpAddressByte)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadRegisters (Ok regs) ->
            ( { model
                | modDataUpdate = regs
                , status = AllGood
              }
            , Cmd.none
            )

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

        ReceivedConnectionInfo (Ok Nothing) ->
            ( model, getTimeZone )

        ReceivedConnectionInfo (Err err) ->
            ( { model | status = Bad <| showHttpError err }, Cmd.none )

        RefreshRequest regs ->
            ( { model | status = Loading }, updateModDataRequest regs )

        ConnectRequest ->
            ( { model | connectStatus = Connecting }, connectRequest model )

        ConnectedResponse (Ok _) ->
            ( { model | connectStatus = Connected }, Cmd.none )

        ConnectedResponse (Err err) ->
            ( { model
                | status = Bad <| showHttpError err
                , connectStatus = Connect
              }
            , Cmd.none
            )

        ChangeIpAddress byte str ->
            if String.isEmpty str then
                ( { model | ipAddress = setIpAddressByte byte model.ipAddress Nothing }, Cmd.none )

            else
                case String.toInt str of
                    Nothing ->
                        ( model, Cmd.none )

                    Just b ->
                        if b < 0 || b > 255 then
                            ( model, Cmd.none )

                        else
                            ( { model | ipAddress = setIpAddressByte byte model.ipAddress (Just b) }, Cmd.none )

        ChangePort portNum ->
            if String.isEmpty portNum then
                ( { model | socketPort = Nothing }, Cmd.none )

            else
                case String.toInt portNum of
                    Nothing ->
                        ( model, Cmd.none )

                    Just p ->
                        if p < 0 || p > 65535 then
                            ( model, Cmd.none )

                        else
                            ( { model | socketPort = Just p }, Cmd.none )

        ChangeTimeout tm ->
            if String.isEmpty tm then
                ( { model | timeout = Nothing }, Cmd.none )

            else
                case String.toInt tm of
                    Nothing ->
                        ( model, Cmd.none )

                    Just t ->
                        if t < 0 || t > 65535 then
                            ( model, Cmd.none )

                        else
                            ( { model | timeout = Just t }, Cmd.none )

        DisconnectRequest ->
            case model.connectStatus of
                Connected ->
                    ( { model | connectStatus = Disconnecting }, disconnectRequest )

                _ ->
                    ( model, Cmd.none )

        DisconnectedResponse (Ok _) ->
            ( { model | connectStatus = Connect }, Cmd.none )

        DisconnectedResponse (Err err) ->
            ( { model
                | status = Bad <| showHttpError err
                , connectStatus = Connect
              }
            , Cmd.none
            )

        ChangeActiveTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        CsvRequested ->
            ( model, Select.file [] CsvSelected )

        CsvSelected file ->
            ( { model | csvFileName = Just <| File.name file }, Task.perform CsvLoaded (File.toString file) )

        CsvLoaded content ->
            ( { model
                | csvContent = Just content
                , csvLoaded = False
              }
            , Cmd.none
            )

        ModDataRequest ->
            ( model, requestModData model )

        ReceivedModData (Err err) ->
            ( { model | status = Bad <| showHttpError err }, Cmd.none )

        ReceivedModData (Ok md) ->
            ( { model
                | modDataUpdate = newModDataUpdate md
                , csvLoaded = True
                , selectAllCheckbox = False
                , selectSome = False
              }
            , Cmd.none
            )

        -- Set the flag select All in the ModData table
        SelectAllChecked b ->
            case model.activeTab of
                ModDataTable ->
                    ( { model
                        | selectAllCheckbox = b
                        , selectSome = b
                        , modDataUpdate = List.map (\mdu -> { mdu | mduSelected = b }) model.modDataUpdate
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | selectAllCheckbox = b }, Cmd.none )

        ModDataChecked idx checked ->
            let
                newMd =
                    List.indexedMap (replaceModDataSelected idx checked) model.modDataUpdate
            in
            ( { model
                | modDataUpdate = newMd
                , selectSome = List.any (\mdu -> mdu.mduSelected) newMd
              }
            , Cmd.none
            )

        -- Toggles the write all button in the ModData tab
        ToggleWriteAll b ->
            case model.activeTab of
                ModDataTable ->
                    ( { model
                        | readWriteAll = b
                        , modDataUpdate =
                            List.map
                                (\mdu ->
                                    if writeableReg mdu.mduModData then
                                        { mdu | mduRW = b }

                                    else
                                        mdu
                                )
                                model.modDataUpdate
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- Toggles the write flag in a single modData in the modData table
        ModDataWrite idx flag ->
            let
                newMd =
                    List.indexedMap (replaceModDataWrite idx flag) model.modDataUpdate
            in
            ( { model | modDataUpdate = newMd }, Cmd.none )

        -- Change the value of a mod data inside the Mod Data tab
        ChangeModDataValue idx str ->
            let
                -- get an array of the ModDataUpdate
                arrMDU =
                    Array.fromList model.modDataUpdate

                -- Maybe a moddataUpdate from the array
                maybeMDU =
                    Array.get idx arrMDU

                -- change the Modvamue of the Maybe modData
                newMaybeMd =
                    Maybe.map (\mdu -> { mdu | mduModData = fromModType mdu.mduModData str }) maybeMDU
            in
            case newMaybeMd of
                Nothing ->
                    ( model, Cmd.none )

                Just md ->
                    ( { model
                        | modDataUpdate = Array.toList <| Array.set idx md arrMDU
                      }
                    , Cmd.none
                    )

        -- Expand the status bar
        ExpandStatus ->
            case model.statusBarState of
                Expanded ->
                    ( { model | statusBarState = Retracted }, Cmd.none )

                Retracted ->
                    ( { model | statusBarState = Expanded }, Cmd.none )

        TimeZone zone ->
            ( { model | timeZone = zone }, getPosixTime )

        -- Get current posix
        NewTime time ->
            ( { model | timePosix = time }, Cmd.none )


fromModType : ModData -> String -> ModData
fromModType md str =
    case md.modValue of
        ModWord _ ->
            { md | modValue = ModWord <| String.toInt str }

        ModFloat _ ->
            { md | modValue = ModFloat <| toMFloat str }


initCmd : Cmd Msg
initCmd =
    connectionInfoRequest


connectionInfoRequest : Cmd Msg
connectionInfoRequest =
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
        Http.BadUrl s ->
            s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus s ->
            "Bad status " ++ String.fromInt s

        Http.BadBody s ->
            s


updateModDataRequest : List ModDataUpdate -> Cmd Msg
updateModDataRequest regs =
    Http.post
        { url = "http://localhost:4000/modData"
        , body = Http.jsonBody <| E.list encodeModDataUpdate regs
        , expect = Http.expectJson ReadRegisters <| D.list decodeModDataUpdate
        }


requestModData : Model -> Cmd Msg
requestModData model =
    Http.post
        { url = "http://localhost:4000/parseModData"
        , body = Http.jsonBody <| E.string <| Maybe.withDefault "" model.csvContent
        , expect = Http.expectJson ReceivedModData <| D.list decodeModData
        }


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform TimeZone Time.here


getPosixTime : Cmd Msg
getPosixTime =
    Task.perform NewTime Time.now
