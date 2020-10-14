module Update exposing (getPosixTime, getTimeZone, initCmd, update)

import Array
import Browser.Dom as Dom
import File
import File.Select as Select
import Http
import Json.Decode as D
import Json.Encode as E
import Notifications
    exposing
        ( Notification
        , NotificationState(..)
        , StatusBarState(..)
        , changeNotificationState
        )
import Settings exposing
    ( Setting
    , SettingStatus(..)
    , SettingInputUpdateValue(..)
    , updateCheckboxSetting
    )
import Task
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , ConnectionInfo(..)
        , ModDataUpdate
        , ModValue(..)
        , Model
        , Msg(..)
        , RegType(..)
        , decodeConnInfo
        , decodeModData
        , decodeModDataUpdate
        , encodeModDataUpdate
        , encodeTCPConnectionInfo
        , fromModType
        , newModDataUpdate
        , replaceModDataSelected
        , replaceModDataWrite
        , showConnInfo
        , writeableReg
        )
import Types.IpAddress exposing (setIpAddressByte)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadRegisters (Ok regs) ->
            ( { model
                | modDataUpdate = regs
                , notifications = simpleNot model "Selected registers updated"
              }
            , jumpToBottom "status"
            )

        ReadRegisters (Err err) ->
            ( { model
                | notifications =
                    detailedNot
                        model
                        "Error reading registers"
                        (showHttpError err)
              }
            , jumpToBottom "status"
            )

        ReceivedConnectionInfo (Ok (Just conn)) ->
            ( updateConnInfoModel model conn
            , jumpToBottom "status"
            )

        ReceivedConnectionInfo (Ok Nothing) ->
            ( { model
                | notifications = simpleNot model "Not Connected"
              }
            , jumpToBottom "status"
            )

        ReceivedConnectionInfo (Err err) ->
            ( { model
                | notifications =
                    detailedNot
                        model
                        "Error receiving connection info"
                        (showHttpError err)
              }
            , jumpToBottom "status"
            )

        RefreshRequest regs ->
            ( { model | notifications = simpleNot model "Updating registers" }
            , updateModDataRequest regs
            )

        ConnectRequest ->
            ( { model
                | connectStatus = Connecting
              }
            , connectRequest model
            )

        ConnectedResponse (Ok _) ->
            ( { model
                | connectStatus = Connected
                , notifications = simpleNot model "Connected"
              }
            , jumpToBottom "status"
            )

        ConnectedResponse (Err err) ->
            ( { model
                | connectStatus = Connect
                , notifications =
                    detailedNot
                        model
                        "Error connecting to client"
                        (showHttpError err)
              }
            , jumpToBottom "status"
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
            ( { model
                | connectStatus = Connect
                , notifications = simpleNot model "Disconnected"
              }
            , jumpToBottom "status"
            )

        DisconnectedResponse (Err err) ->
            ( { model
                | connectStatus = Connect
                , notifications =
                    detailedNot
                        model
                        "Error disconnectiing from client"
                        (showHttpError err)
              }
            , jumpToBottom "status"
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
                , notifications = simpleNot model "Loaded register table from disk"
              }
            , jumpToBottom "status"
            )

        ModDataRequest ->
            ( model, requestModData model )

        ReceivedModData (Err err) ->
            ( { model
                | notifications =
                    detailedNot
                        model
                        "Error parsing register table"
                        (showHttpError err)
              }
            , jumpToBottom "status"
            )

        ReceivedModData (Ok md) ->
            ( { model
                | modDataUpdate = newModDataUpdate md
                , csvLoaded = True
                , selectAllCheckbox = False
                , selectSome = False
                , notifications = simpleNot model "Register table updated"
              }
            , jumpToBottom "status"
            )

        -- Set the flag select All in the ModData table
        SelectAllChecked b ->
            case model.activeTab of
                ModDataTab ->
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
                ModDataTab ->
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
            ( { model | timeZone = zone }, initTime )

        -- Get current posix
        NewTime time ->
            ( { model | timePosix = time }, Cmd.none )

        -- Part of initialisation sequence,
        -- will only be run once per page load
        InitTime time ->
            ( { model | timePosix = time }, connectionInfoRequest )

        ExpandNotification not ->
            ( { model
                | notifications = changeNotificationState not model.notifications
              }
            , Cmd.none
            )

        SetActiveSetting setting ->
            ( updateActiveSettingModel model setting
            , Cmd.none
            )

        DummyCheckboxMsg settingIdx inputIdx flag ->
            ( updateDummy model settingIdx inputIdx flag, Cmd.none )

        DummyNumberInputMsg settingIdx inputIdx valueStr ->
            ( updateDummyInput model settingIdx inputIdx valueStr, Cmd.none)

        NoOp ->
            ( model, Cmd.none )


updateDummy :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> Bool -- flag
    -> Model
updateDummy model settingIdx inputIdx newFlag =
    case updateCheckboxSetting model.settings settingIdx inputIdx (CheckBoxValue newFlag) of
        Nothing ->
            { model | keepAlive = newFlag }

        Just modifiedSettings ->
            { model
                | keepAlive = newFlag
                , settings = modifiedSettings
            }
updateDummyInput :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> String
    -> Model
updateDummyInput model settingIdx inputIdx valueStr =
    case String.toInt valueStr of
        Nothing -> model
        Just newInt ->
            case updateCheckboxSetting model.settings settingIdx inputIdx (NumberInputValue newInt) of
                Nothing ->
                    { model | keepAliveInterval = newInt }

                Just modifiedSettings ->
                    { model
                        | keepAliveInterval = newInt
                        , settings = modifiedSettings
                    }


initCmd : Cmd Msg
initCmd =
    getTimeZone


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
        , body = Http.jsonBody <| encodeTCPConnectionInfo model
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



-- Initialise time, will only be run on page reload


initTime : Cmd Msg
initTime =
    Task.perform InitTime Time.now


getPosixTime : Cmd Msg
getPosixTime =
    Task.perform NewTime Time.now


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


simpleNot : Model -> String -> List Notification
simpleNot model header =
    Notification
        model.timePosix
        header
        Nothing
        NotifRetracted
        :: model.notifications


detailedNot : Model -> String -> String -> List Notification
detailedNot model header detailed =
    Notification
        model.timePosix
        header
        (Just detailed)
        NotifRetracted
        :: model.notifications



------------------------------------------------------------------------------------------------------------------
-- Model update


updateConnInfoModel : Model -> ConnectionInfo -> Model
updateConnInfoModel model connInfo =
    case connInfo of
        TCPConnectionInfo tcp ->
            { model
                | ipAddress = tcp.ipAddress
                , socketPort = Just tcp.socketPort
                , serialPort = Nothing
                , timeout = Just tcp.timeout
                , connectStatus = Connected
                , notifications =
                    detailedNot
                        model
                        "Connected"
                        (showConnInfo connInfo)
            }

        RTUConnectionInfo rtu ->
            { model
                | socketPort = Nothing
                , serialPort = Just rtu.rtuAddress
                , timeout = Just rtu.timeout
                , connectStatus = Connected
                , notifications =
                    detailedNot
                        model
                        "Connected"
                        (showConnInfo connInfo)
            }


updateActiveSettingModel : Model -> Setting Msg -> Model
updateActiveSettingModel model setting =
    let
        newSettings =
            List.map
                (\set ->
                    if set.description == setting.description then
                        { set | status = Active }

                    else
                        { set | status = NotActive }
                )
                model.settings
    in
    { model | settings = newSettings }
