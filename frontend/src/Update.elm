module Update exposing (getPosixTime, getTimeZone, initCmd, update)

import Array
import Browser.Dom as Dom
import Element exposing (onLeft)
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
import Settings
    exposing
        ( Setting
        , SettingInputUpdateValue(..)
        , SettingStatus(..)
        , updateCheckboxSetting
        )
import Task
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , ConnectionInfo(..)
        , KeepAliveResponse(..)
        , ModData
        , ModDataUpdate
        , ModValue(..)
        , Model
        , Msg(..)
        , ReadWrite
        , RegType(..)
        , decodeConnInfo
        , decodeKeepAliveResponse
        , decodeModData
        , decodeModDataUpdate
        , encodeKeepAlive
        , encodeModDataUpdate
        , encodeTCPConnectionInfo
        , encodeTCPConnectionRequest
        , fromModType
        , newModDataUpdate
        , replaceModDataSelected
        , replaceModDataWrite
        , showConnInfo
        , showKeepAliveResponse
        , writeableReg
        )
import Types.IpAddress exposing (IpAddressByte, setIpAddressByte)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadRegisters response ->
            ( readRegistersModelUpdate model response
            , jumpToBottom "status"
            )

        ReceivedConnectionInfo result ->
            ( receivedConnectionInfoModelUpdate model result
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

        ConnectedResponse result ->
            ( connectedResponseModelUpdate model result
            , jumpToBottom "status"
            )

        ChangeIpAddress byte str ->
            ( changeIpAddressModelUpdate model byte str
            , Cmd.none
            )

        ChangePort portNum ->
            ( changePortModelUpdate model portNum
            , Cmd.none
            )

        ChangeTimeout tm ->
            ( changeTimeoutModelUpdate model tm
            , Cmd.none
            )

        DisconnectRequest ->
            case model.connectStatus of
                Connected ->
                    ( { model | connectStatus = Disconnecting }, disconnectRequest )

                _ ->
                    ( model, Cmd.none )

        DisconnectedResponse result ->
            ( disconnectedResponseModelUpdate model result
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

        ReceivedModData result ->
            ( receivedModDataModelUpdate model result
            , jumpToBottom "status"
            )

        -- Set the flag select All in the ModData table
        SelectAllChecked b ->
            ( selectAllCheckedModelUpdate model b
            , Cmd.none
            )

        ModDataChecked idx checked ->
            ( modDataCheckedModelUpdate model idx checked
            , Cmd.none
            )

        -- Toggles the write all button in the ModData tab
        ToggleWriteAll b ->
            ( toggleWriteAllModelUpdate model b
            , Cmd.none
            )

        -- Toggles the write flag in a single modData in the modData table
        ModDataWrite idx rw ->
            ( modDataWriteModelUpdate model idx rw
            , Cmd.none
            )

        -- Change the value of a mod data inside the Mod Data tab
        ChangeModDataValue idx str ->
            ( changeModDataValueModelUpdate model idx str
            , Cmd.none
            )

        -- Expand the status bar
        ExpandStatus ->
            ( expandStatusModelUpdate model
            , Cmd.none
            )

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
            ( activeSettingModelUpdate model setting
            , Cmd.none
            )

        KeepAliveMsg settingIdx inputIdx flag ->
            ( keepAliveModelUpdate model settingIdx inputIdx flag, keepAliveRequest model flag )

        KeepAliveIdleMsg settingIdx inputIdx valueStr ->
            ( keepAliveIdleModelUpdate model settingIdx inputIdx valueStr, Cmd.none )

        KeepAliveIntervalMsg settingIdx inputIdx valueStr ->
            ( keepAliveIntervalModelUpdate model settingIdx inputIdx valueStr, Cmd.none )

        KeepAliveResponseMsg response ->
            ( updateKeepAliveResponseModel model response, jumpToBottom "status" )

        NoOp ->
            ( model, Cmd.none )



------------------------------------------------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------------------------------------------------


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
        , body = Http.jsonBody <| encodeTCPConnectionRequest model
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



-- Send keep alive flag seperately, as the model might not update in time


keepAliveRequest : Model -> Bool -> Cmd Msg
keepAliveRequest model flag =
    case model.connectStatus of
        Connected ->
            Http.post
                { url = "http://localhost:4000/keepAlive"
                , body = Http.jsonBody <| encodeKeepAlive model flag
                , expect = Http.expectJson KeepAliveResponseMsg decodeKeepAliveResponse
                }

        _ ->
            Cmd.none


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
-- Model updates
------------------------------------------------------------------------------------------------------------------


readRegistersModelUpdate : Model -> Result Http.Error (List ModDataUpdate) -> Model
readRegistersModelUpdate model result =
    case result of
        Ok regs ->
            { model
                | modDataUpdate = regs
                , notifications = simpleNot model "Selected registers updated"
            }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error reading registers"
                        (showHttpError err)
            }


receivedConnectionInfoModelUpdate : Model -> Result Http.Error (Maybe ConnectionInfo) -> Model
receivedConnectionInfoModelUpdate model result =
    case result of
        Ok (Just connInfo) ->
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

        Ok Nothing ->
            { model
                | notifications = simpleNot model "Not Connected"
            }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error receiving connection info"
                        (showHttpError err)
            }


connectedResponseModelUpdate : Model -> Result Http.Error () -> Model
connectedResponseModelUpdate model result =
    case result of
        Ok _ ->
            { model
                | connectStatus = Connected
                , notifications = simpleNot model "Connected"
            }

        Err err ->
            { model
                | connectStatus = Connect
                , notifications =
                    detailedNot
                        model
                        "Error connecting to client"
                        (showHttpError err)
            }


changeIpAddressModelUpdate : Model -> IpAddressByte -> String -> Model
changeIpAddressModelUpdate model byte str =
    if String.isEmpty str then
        { model | ipAddress = setIpAddressByte byte model.ipAddress Nothing }

    else
        case String.toInt str of
            Nothing ->
                model

            Just b ->
                if b < 0 || b > 255 then
                    model

                else
                    { model | ipAddress = setIpAddressByte byte model.ipAddress (Just b) }


changePortModelUpdate : Model -> String -> Model
changePortModelUpdate model str =
    if String.isEmpty str then
        { model | socketPort = Nothing }

    else
        case String.toInt str of
            Nothing ->
                model

            Just p ->
                if p < 0 || p > 65535 then
                    model

                else
                    { model | socketPort = Just p }


activeSettingModelUpdate : Model -> Setting Msg -> Model
activeSettingModelUpdate model setting =
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


changeTimeoutModelUpdate : Model -> String -> Model
changeTimeoutModelUpdate model str =
    if String.isEmpty str then
        { model | timeout = Nothing }

    else
        case String.toInt str of
            Nothing ->
                model

            Just t ->
                if t < 0 || t > 65535 then
                    model

                else
                    { model | timeout = Just t }


disconnectedResponseModelUpdate : Model -> Result Http.Error () -> Model
disconnectedResponseModelUpdate model result =
    case result of
        Ok _ ->
            { model
                | connectStatus = Connect
                , notifications = simpleNot model "Disconnected"
            }

        Err err ->
            { model
                | connectStatus = Connect
                , notifications =
                    detailedNot
                        model
                        "Error disconnectiing from client"
                        (showHttpError err)
            }


receivedModDataModelUpdate : Model -> Result Http.Error (List ModData) -> Model
receivedModDataModelUpdate model result =
    case result of
        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error parsing register table"
                        (showHttpError err)
            }

        Ok md ->
            { model
                | modDataUpdate = newModDataUpdate md
                , csvLoaded = True
                , selectAllCheckbox = False
                , selectSome = False
                , notifications = simpleNot model "Register table updated"
            }


selectAllCheckedModelUpdate : Model -> Bool -> Model
selectAllCheckedModelUpdate model b =
    case model.activeTab of
        ModDataTab ->
            { model
                | selectAllCheckbox = b
                , selectSome = b
                , modDataUpdate = List.map (\mdu -> { mdu | mduSelected = b }) model.modDataUpdate
            }

        _ ->
            { model | selectAllCheckbox = b }


keepAliveModelUpdate :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> Bool
    -> Model
keepAliveModelUpdate model settingIdx inputIdx newFlag =
    case updateCheckboxSetting model.settings settingIdx inputIdx (CheckBoxValue newFlag) of
        Nothing ->
            { model | keepAlive = newFlag }

        Just modifiedSettings ->
            { model
                | keepAlive = newFlag
                , settings = modifiedSettings
            }


modDataCheckedModelUpdate : Model -> Int -> Bool -> Model
modDataCheckedModelUpdate model idx checked =
    let
        newMd =
            List.indexedMap (replaceModDataSelected idx checked) model.modDataUpdate
    in
    { model
        | modDataUpdate = newMd
        , selectSome = List.any (\mdu -> mdu.mduSelected) newMd
    }


keepAliveIdleModelUpdate :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> String
    -> Model
keepAliveIdleModelUpdate model settingIdx inputIdx valueStr =
    let
        checkedValue =
            String.toInt valueStr
    in
    case updateCheckboxSetting model.settings settingIdx inputIdx (NumberInputValue checkedValue) of
        Nothing ->
            { model | keepAliveIdle = checkedValue }

        Just modifiedSettings ->
            { model
                | keepAliveIdle = checkedValue
                , settings = modifiedSettings
            }


toggleWriteAllModelUpdate : Model -> ReadWrite -> Model
toggleWriteAllModelUpdate model b =
    case model.activeTab of
        ModDataTab ->
            { model
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

        _ ->
            model


modDataWriteModelUpdate : Model -> Int -> ReadWrite -> Model
modDataWriteModelUpdate model idx rw =
    let
        newMd =
            List.indexedMap (replaceModDataWrite idx rw) model.modDataUpdate
    in
    { model | modDataUpdate = newMd }


keepAliveIntervalModelUpdate :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> String
    -> Model
keepAliveIntervalModelUpdate model settingIdx inputIdx valueStr =
    let
        checkedValue =
            String.toInt valueStr
    in
    case updateCheckboxSetting model.settings settingIdx inputIdx (NumberInputValue checkedValue) of
        Nothing ->
            { model | keepAliveInterval = checkedValue }

        Just modifiedSettings ->
            { model
                | keepAliveInterval = checkedValue
                , settings = modifiedSettings
            }


changeModDataValueModelUpdate : Model -> Int -> String -> Model
changeModDataValueModelUpdate model idx str =
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
            model

        Just md ->
            { model
                | modDataUpdate = Array.toList <| Array.set idx md arrMDU
            }

expandStatusModelUpdate : Model -> Model
expandStatusModelUpdate model =
    case model.statusBarState of
        Expanded ->
            { model | statusBarState = Retracted }

        Retracted ->
            { model | statusBarState = Expanded }


updateKeepAliveResponseModel : Model -> Result Http.Error KeepAliveResponse -> Model
updateKeepAliveResponseModel model response =
    case response of
        Ok message ->
            { model | notifications = simpleNot model (showKeepAliveResponse message) }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error updating keep alive setting"
                        (showHttpError err)
            }
