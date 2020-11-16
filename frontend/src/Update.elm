module Update exposing (getPosixTime, getTimeZone, initCmd, update)

import Array
import Browser.Dom as Dom
import Dropdown exposing (Option, retract, setDropdown, setDropdownExpanded, setDropdownSelected)
import Element exposing (onLeft)
import File
import File.Select as Select
import Html.Attributes exposing (selected)
import Http
import Json.Decode as D
import Json.Encode as E
import ModData
    exposing
        ( ModData
        , ModDataUpdate
        , ModValue(..)
        , RegType(..)
        , decodeModData
        , decodeModDataUpdate
        , encodeModDataUpdate
        , fromModValueInput
        , fromModValueInputUpdate
        , isWriteableReg
        , newModDataUpdate
        , offsetMdu
        , replaceModDataSelected
        , replaceModDataWrite
        , setModValueUpdate
        , setRegAddressUpdate
        , setRegRWUpdate
        , setRegTypeUpdate
        , setRegUidUpdate
        )
import Notifications
    exposing
        ( Notification
        , NotificationState(..)
        , StatusBarState(..)
        , changeNotificationState
        )
import ReadWrite
    exposing
        ( ReadWrite(..)
        )
import Settings
    exposing
        ( Setting
        , SettingInputUpdateValue(..)
        , SettingStatus(..)
        , updateIndexedSetting
        )
import Task
import Time
import Types
    exposing
        ( ActiveTab(..)
        , BaudRate
        , ByteOrder(..)
        , ConnectActiveTab(..)
        , ConnectStatus(..)
        , ConnectionInfo(..)
        , HeartBeat
        , InitInfo
        , KeepAliveResponse(..)
        , Model
        , Msg(..)
        , Parity
        , SettingsOptions(..)
        , StopBits
        , decodeByteOrder
        , decodeConnInfo
        , decodeHeartBeat
        , decodeInitInfo
        , decodeKeepAliveResponse
        , diffList
        , encodeByteOrder
        , encodeHeartBeat
        , encodeKeepAlive
        , encodeRTUConnectionRequest
        , encodeTCPConnectionInfo
        , encodeTCPConnectionRequest
        , fromIdList
        , getSelectedIds
        , replaceHeartBeatSelected
        , retractDropdowns
        , showByteOrderResponse
        , showConnInfo
        , showFailedHeartBeat
        , showKeepAliveResponse
        , showOs
        , toByteOrder
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
            ( changeActiveTabModelUpdate model tab
            , Cmd.none
            )

        CsvRequested ->
            ( model, Select.file [] CsvSelected )

        CsvSelected file ->
            ( { model | csvFileName = Just <| File.name file }, Task.perform CsvLoaded (File.toString file) )

        CsvLoaded content ->
            ( { model
                | csvContent = Just content
                , csvLoaded = False
              }
            , requestModData content
            )

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
            ( { model | timeZone = zone }
            , initTime
            )

        -- Get current posix
        NewTime time ->
            ( { model | timePosix = time }
            , Cmd.none
            )

        -- Part of initialisation sequence,
        -- will only be run once per page load
        InitTime time ->
            ( { model | timePosix = time }
            , Cmd.none
            )

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
            ( keepAliveModelUpdate model settingIdx inputIdx flag
            , keepAliveRequest model flag
            )

        KeepAliveIdleMsg settingIdx inputIdx valueStr ->
            ( keepAliveIdleModelUpdate model settingIdx inputIdx valueStr
            , Cmd.none
            )

        KeepAliveIntervalMsg settingIdx inputIdx valueStr ->
            ( keepAliveIntervalModelUpdate model settingIdx inputIdx valueStr
            , Cmd.none
            )

        KeepAliveResponseMsg response ->
            ( updateKeepAliveResponseModel model response
            , jumpToBottom "status"
            )

        ChangeByteOrderMsg settingIdx inputIdx setting ->
            ( changeByteOrderModelUpdate model settingIdx inputIdx setting
            , changeByteOrderRequest (toByteOrder setting)
            )

        ChangeByteOrderResponse result ->
            ( changeByteOrderResponseModelUpdate model result
            , jumpToBottom "status"
            )

        RegRegTypeDrop opt ->
            ( regTypeDropModelUpdate model opt
            , Cmd.none
            )

        RegValueTypeDrop opt ->
            ( { model
                | regModValueDd = setDropdown model.regModValueDd opt
                , regMdu = setModValueUpdate model.regMdu opt.value
                , regTypeDd = retract model.regTypeDd
              }
            , Cmd.none
            )

        RegAddress str ->
            ( regAddressModelUpdate model str
            , Cmd.none
            )

        RegUid str ->
            ( regUidModelUpdate model str
            , Cmd.none
            )

        RegToggleRW rw ->
            ( regToggleRWModelUpdate model rw
            , Cmd.none
            )

        RegNumber str ->
            ( { model | regNumReg = String.toInt str }
            , Cmd.none
            )

        RegModValue str ->
            ( regModValueModelUpdate model str
            , Cmd.none
            )

        UpdateRegMdu ->
            ( { model
                | regModValueDd = retract model.regModValueDd
                , regTypeDd = retract model.regTypeDd
              }
            , updateRegMdu model
            )

        UpdateRegMduResponse response ->
            ( updateRegMduModelUpdate model response
            , jumpToBottom "status"
            )

        ReceivedInitInfo response ->
            ( initInfoModelUpdate model response
            , jumpToBottom "status"
            )

        ChangeActiveConnectTab tab ->
            ( { model | connActiveTab = tab }
            , Cmd.none
            )

        ChangeSerialPort str ->
            ( changeSerialPortModelUpdate model str
            , Cmd.none
            )

        BaudRateDrop opt ->
            ( baudRateDdModelUpdate model opt
            , Cmd.none
            )

        StopBitsDrop opt ->
            ( stopBitsModelUpdate model opt
            , Cmd.none
            )

        ParityDrop opt ->
            ( parityModelUpdate model opt
            , Cmd.none
            )

        -- HeartBeat
        HeartUid uid ->
            ( heartUidModelUpdate model uid
            , Cmd.none
            )

        HeartAddress addr ->
            ( heartAddrModelUpdate model addr
            , Cmd.none
            )

        HeartInterval intv ->
            ( heartIntvModelUpdate model intv
            , Cmd.none
            )

        StartHeartBeat ->
            startHeartBeat model

        StopHeartBeat ->
            stopHeartBeat model

        UpdateActiveHeartBeats result ->
            updateActiveHeartBeats model result

        HeartBeatChecked idx flag ->
            ( hbCheckedModelUpdate model idx flag
            , Cmd.none
            )

        InitHeartBeat result ->
            initHeartBeat model result

        -- Noop
        NoOp ->
            ( model
            , Cmd.none
            )



------------------------------------------------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------------------------------------------------


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ getTimeZone
        , initRequest
        , initHeartBeatRequest
        ]


initRequest : Cmd Msg
initRequest =
    Http.get
        { url = "http://localhost:4000/init"
        , expect = Http.expectJson ReceivedInitInfo decodeInitInfo
        }


connectionInfoRequest : Cmd Msg
connectionInfoRequest =
    Http.get
        { url = "http://localhost:4000/connectInfo"
        , expect = Http.expectJson ReceivedConnectionInfo (D.maybe decodeConnInfo)
        }


connectRequest : Model -> Cmd Msg
connectRequest model =
    let
        encodeRequest =
            case model.connActiveTab of
                TCPTab ->
                    encodeTCPConnectionRequest model

                RTUTab ->
                    encodeRTUConnectionRequest model
    in
    Http.post
        { url = "http://localhost:4000/connect"
        , body = Http.jsonBody <| encodeRequest
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


requestModData : String -> Cmd Msg
requestModData content =
    Http.post
        { url = "http://localhost:4000/parseModData"
        , body = Http.jsonBody <| E.string content
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


changeByteOrderRequest : ByteOrder -> Cmd Msg
changeByteOrderRequest order =
    Http.post
        { url = "http://localhost:4000/byteOrder"
        , body = Http.jsonBody <| encodeByteOrder order
        , expect = Http.expectJson ChangeByteOrderResponse decodeByteOrder
        }


updateRegMdu : Model -> Cmd Msg
updateRegMdu model =
    Http.post
        { url = "http://localhost:4000/modData"
        , body = Http.jsonBody <| E.list encodeModDataUpdate <| getRegMduList model
        , expect = Http.expectJson UpdateRegMduResponse <| D.list decodeModDataUpdate
        }


getRegMduList : Model -> List ModDataUpdate
getRegMduList model =
    case model.regMdu.mduRW of
        Write ->
            [ model.regMdu ]

        Read ->
            case model.regNumReg of
                Nothing ->
                    []

                Just num ->
                    offsetMdu model.regMdu num


sendHeartBeats : HeartBeat -> Cmd Msg
sendHeartBeats hb =
    Http.post
        { url = "http://localhost:4000/startHeartbeat"
        , body = Http.jsonBody <| encodeHeartBeat <| hb
        , expect = Http.expectJson UpdateActiveHeartBeats <| D.list D.int
        }


stopHeartBeatRequest : List HeartBeat -> Cmd Msg
stopHeartBeatRequest hbs =
    Http.post
        { url = "http://localhost:4000/stopHeartbeat"
        , body = Http.jsonBody <| E.list E.int <| getSelectedIds hbs
        , expect = Http.expectJson UpdateActiveHeartBeats <| D.list D.int
        }



-- Initial request to the server to provide any running heartbeat signals
-- Nedded when the frontend is reloaded


initHeartBeatRequest : Cmd Msg
initHeartBeatRequest =
    Http.post
        { url = "http://localhost:4000/initHeartbeat"
        , body = Http.emptyBody
        , expect = Http.expectJson InitHeartBeat <| D.list decodeHeartBeat
        }



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
        Ok mconninfo ->
            connInfoModelUpdate model mconninfo

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


activeSettingModelUpdate : Model -> Setting SettingsOptions Msg -> Model
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


changeActiveTabModelUpdate : Model -> ActiveTab -> Model
changeActiveTabModelUpdate model tab =
    let
        retractedModel =
            retractDropdowns model
    in
    { retractedModel
        | activeTab = tab
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

        HeartbeatTab ->
            { model
                | heartSelectAll = b
                , heartSelectSome = b
                , heartbeats = List.map (\hb -> { hb | selected = b }) model.heartbeats
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
    case updateIndexedSetting model.settings settingIdx inputIdx (CheckBoxValue newFlag) of
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
    case updateIndexedSetting model.settings settingIdx inputIdx (NumberInputValue checkedValue) of
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
                            if isWriteableReg mdu.mduModData.modRegType then
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
    case updateIndexedSetting model.settings settingIdx inputIdx (NumberInputValue checkedValue) of
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
            Maybe.map (\mdu -> { mdu | mduModData = fromModValueInput mdu.mduModData str }) maybeMDU
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


changeByteOrderModelUpdate : Model -> Int -> Int -> SettingsOptions -> Model
changeByteOrderModelUpdate model settingIdx inputIdx option =
    let
        order =
            toByteOrder option
    in
    case updateIndexedSetting model.settings settingIdx inputIdx (RadioValue (Just option)) of
        Nothing ->
            model

        Just modifiedSettings ->
            { model
                | byteOrder = order
                , settings = modifiedSettings
            }


changeByteOrderResponseModelUpdate : Model -> Result Http.Error ByteOrder -> Model
changeByteOrderResponseModelUpdate model result =
    case result of
        Ok order ->
            { model | notifications = simpleNot model (showByteOrderResponse order) }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error updating byte order setting"
                        (showHttpError err)
            }


regTypeDropModelUpdate : Model -> Option RegType Msg -> Model
regTypeDropModelUpdate model opt =
    { model
        | regTypeDd = setDropdown model.regTypeDd opt
        , regMdu = setRegTypeUpdate model.regMdu opt.value
        , regModValueDd = retract model.regModValueDd
    }


regAddressModelUpdate : Model -> String -> Model
regAddressModelUpdate model str =
    { model
        | regAddress = String.toInt str
        , regMdu = setRegAddressUpdate model.regMdu <| Maybe.withDefault 0 (String.toInt str)
    }


regUidModelUpdate : Model -> String -> Model
regUidModelUpdate model str =
    { model
        | regUid = String.toInt str
        , regMdu = setRegUidUpdate model.regMdu <| Maybe.withDefault 1 (String.toInt str)
    }


regToggleRWModelUpdate : Model -> ReadWrite -> Model
regToggleRWModelUpdate model rw =
    if isWriteableReg model.regMdu.mduModData.modRegType then
        { model
            | regMdu = setRegRWUpdate model.regMdu rw
        }

    else
        { model
            | regMdu = setRegRWUpdate model.regMdu Read
        }


regModValueModelUpdate : Model -> String -> Model
regModValueModelUpdate model str =
    { model | regMdu = fromModValueInputUpdate model.regMdu str }


updateRegMduModelUpdate : Model -> Result Http.Error (List ModDataUpdate) -> Model
updateRegMduModelUpdate model result =
    case result of
        Ok mdus ->
            { model
                | notifications = simpleNot model "Register request retrieved"
                , regResponse = mdus
            }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error retriving requested registers"
                        (showHttpError err)
            }


initInfoModelUpdate : Model -> Result Http.Error InitInfo -> Model
initInfoModelUpdate model result =
    case result of
        Ok info ->
            let
                connmodel =
                    connInfoModelUpdate model info.initConnInfo
            in
            { connmodel
                | os = info.initOS
                , notifications =
                    simpleNot connmodel <|
                        "Server operating system: "
                            ++ showOs info.initOS
            }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error retriving init info from server"
                        (showHttpError err)
            }


connInfoModelUpdate : Model -> Maybe ConnectionInfo -> Model
connInfoModelUpdate model mconninfo =
    case mconninfo of
        Just conninfo ->
            case conninfo of
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
                                (showConnInfo conninfo)
                    }

                RTUConnectionInfo rtu ->
                    { model
                        | socketPort = Nothing
                        , serialPort = Just rtu.rtuAddress
                        , baudrate = rtu.serialSettings.baudRate
                        , baudrateDd = setDropdownSelected model.baudrateDd rtu.serialSettings.baudRate
                        , stopBits = rtu.serialSettings.stopBits
                        , stopBitsDd = setDropdownSelected model.stopBitsDd rtu.serialSettings.stopBits
                        , parity = rtu.serialSettings.parity
                        , parityDd = setDropdownSelected model.parityDd rtu.serialSettings.parity
                        , timeout = Just <| rtu.serialSettings.timeout
                        , connectStatus = Connected
                        , notifications =
                            detailedNot
                                model
                                "Connected"
                                (showConnInfo conninfo)
                    }

        Nothing ->
            { model
                | notifications = simpleNot model "Not Connected"
            }


changeSerialPortModelUpdate : Model -> String -> Model
changeSerialPortModelUpdate model str =
    if String.isEmpty str then
        { model | serialPort = Nothing }

    else
        { model | serialPort = Just str }


baudRateDdModelUpdate : Model -> Option BaudRate Msg -> Model
baudRateDdModelUpdate model opt =
    let
        retracted =
            retractDropdowns model
    in
    { retracted
        | baudrateDd = setDropdown model.baudrateDd opt
        , baudrate = opt.value
    }


stopBitsModelUpdate : Model -> Option StopBits Msg -> Model
stopBitsModelUpdate model opt =
    let
        retracted =
            retractDropdowns model
    in
    { retracted
        | stopBitsDd = setDropdown model.stopBitsDd opt
        , stopBits = opt.value
    }


parityModelUpdate : Model -> Option Parity Msg -> Model
parityModelUpdate model opt =
    let
        retracted =
            retractDropdowns model
    in
    { retracted
        | parityDd = setDropdown model.parityDd opt
        , parity = opt.value
    }


heartUidModelUpdate : Model -> String -> Model
heartUidModelUpdate model str =
    { model
        | heartUid = String.toInt str
    }


heartAddrModelUpdate : Model -> String -> Model
heartAddrModelUpdate model str =
    { model
        | heartAddr = String.toInt str
    }


heartIntvModelUpdate : Model -> String -> Model
heartIntvModelUpdate model str =
    { model
        | heartIntv = String.toInt str
    }


startHeartBeat : Model -> ( Model, Cmd Msg )
startHeartBeat model =
    let
        mheartbeat =
            Maybe.map5 HeartBeat
                model.heartUid
                model.heartAddr
                model.heartIntv
                (Just False)
                (Just model.heartId)
    in
    case mheartbeat of
        Nothing ->
            ( model, Cmd.none )

        Just hb ->
            ( { model
                | heartbeats = model.heartbeats ++ [ hb ]

                -- increment id counter
                , heartId = model.heartId + 1
              }
            , sendHeartBeats hb
            )


stopHeartBeat : Model -> ( Model, Cmd Msg )
stopHeartBeat model =
    let
        ( selectedHbs, notSelectedHbs ) =
            List.partition .selected model.heartbeats
    in
    ( { model
        | heartbeats = notSelectedHbs
        , heartSelectAll = False
        , heartSelectSome = False
      }
    , stopHeartBeatRequest selectedHbs
    )


updateActiveHeartBeats : Model -> Result Http.Error (List Int) -> ( Model, Cmd Msg )
updateActiveHeartBeats model result =
    case result of
        Err err ->
            ( { model
                | notifications =
                    detailedNot
                        model
                        "Error updating heartbeat list"
                        (showHttpError err)
              }
            , jumpToBottom "status"
            )

        Ok ids ->
            let
                -- compare the locally stored list with the received ids
                hbs =
                    fromIdList model.heartbeats ids

                diffs =
                    diffList model.heartbeats hbs
            in
            if
                List.isEmpty diffs
                -- All heartbeats are running
            then
                ( { model
                    | heartbeats = hbs
                    , notifications = simpleNot model "Heartbeat signals updated"
                  }
                , jumpToBottom "status"
                )
                -- Some heartbeats failed

            else
                ( { model
                    | heartbeats = hbs
                    , notifications =
                        detailedNot model "Some heartbeat signals have failed" <|
                            List.foldl showFailedHeartBeat "Failed heartbeat signals:\n" diffs
                  }
                , jumpToBottom "status"
                )


initHeartBeat : Model -> Result Http.Error (List HeartBeat) -> ( Model, Cmd Msg )
initHeartBeat model result =
    case result of
        Err err ->
            ( { model
                | notifications =
                    simpleNot
                        model
                        "Error initializing heartbeat signals"
              }
            , jumpToBottom "status"
            )

        Ok heartbeats ->
            ( { model
                | heartbeats = heartbeats
                , heartId =
                    case List.maximum <| List.map .id heartbeats of
                        Nothing ->
                            0

                        Just max ->
                            max + 1
              }
            , Cmd.none
            )


hbCheckedModelUpdate : Model -> Int -> Bool -> Model
hbCheckedModelUpdate model idx flag =
    let
        newHB =
            List.indexedMap (replaceHeartBeatSelected idx flag) model.heartbeats
    in
    { model
        | heartbeats = newHB
        , heartSelectSome = List.any (\hb -> hb.selected) newHB
    }
