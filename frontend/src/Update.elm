module Update exposing (getPosixTime, getTimeZone, initCmd, update)

import Array
import Browser.Dom as Dom
import Dropdown exposing (Option, retract, setDropdown, setDropdownExpanded, setDropdownSelected)
import Element exposing (onLeft)
import File
import File.Select as Select
import Heartbeat exposing (updateSelectedHbType)
import Html.Attributes exposing (selected)
import Http
import Json.Decode as D
import Json.Encode as E
import Maybe
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
        , getRegMduList
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
        , ConnectActiveTab(..)
        , ConnectStatus(..)
        , ConnectionInfo(..)
        , Heartbeat
        , HeartbeatType(..)
        , InitInfo
        , KeepAliveResponse(..)
        , Model
        , Msg(..)
        , Parity
        , SettingsOptions(..)
        , StopBits
        , WordOrder(..)
        , decodeConnInfo
        , decodeHeartbeat
        , decodeInitInfo
        , decodeKeepAliveResponse
        , decodeWordOrder
        , diffList
        , encodeHeartbeat
        , encodeKeepAlive
        , encodeRTUConnectionRequest
        , encodeTCPConnectionInfo
        , encodeTCPConnectionRequest
        , encodeWordOrder
        , fromIdList
        , getSelectedIds
        , replaceHeartbeatSelected
        , retractDropdowns
        , showConnInfo
        , showFailedHeartbeat
        , showKeepAliveResponse
        , showOs
        , showWordOrderResponse
        , toWordOrder
        )
import Types.IpAddress exposing (IpAddressByte, setIpAddressByte)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- init command
        ------------------------------------------------------------------------------------------------------------------
        -- get initial info/configuration from the server
        ReceivedInitInfo response ->
            ( initInfoModelUpdate model response
            , jumpToBottom "status"
            )

        -- Time
        ------------------------------------------------------------------------------------------------------------------
        -- get server timezone
        -- part of initialisation sequence,
        -- will only be run once per page load
        TimeZone zone ->
            ( { model | timeZone = zone }
            , initTime
            )

        -- get server time
        InitTime time ->
            ( { model | timePosix = time }
            , Cmd.none
            )

        -- get current posix
        NewTime time ->
            ( { model | timePosix = time }
            , Cmd.none
            )

        -- Tab management
        ------------------------------------------------------------------------------------------------------------------
        -- active tab management
        ChangeActiveTab tab ->
            ( changeActiveTabModelUpdate model tab
            , Cmd.none
            )

        -- Connect Tab
        ------------------------------------------------------------------------------------------------------------------
        -- change the active connection panel
        ChangeActiveConnectPanel tab ->
            ( { model | connActiveTab = tab }
            , Cmd.none
            )

        -- send a connect request to the server
        ConnectionRequest ->
            ( { model
                | connectStatus = Connecting
              }
            , connectRequest model
            )

        -- connection response
        ConnectionResponse result ->
            ( connectedResponseModelUpdate model result
            , jumpToBottom "status"
            )

        -- disconnect request
        DisconnectRequest ->
            case model.connectStatus of
                Connected ->
                    ( { model | connectStatus = Disconnecting }, disconnectRequest )

                -- ignore if not connected
                _ ->
                    ( model, Cmd.none )

        -- disconnect response
        DisconnectedResponse result ->
            ( disconnectedResponseModelUpdate model result
            , jumpToBottom "status"
            )

        -- TCP Tab
        ------------------------------------------------------------------------------------------------------------------
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

        -- RTU Tab
        ------------------------------------------------------------------------------------------------------------------
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

        -- Registers Tab
        ------------------------------------------------------------------------------------------------------------------
        RegRegTypeDrop opt ->
            ( regTypeDropModelUpdate model opt
            , Cmd.none
            )

        RegValueTypeDrop opt ->
            ( regValueTypeModelUpdate model opt
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
            ( regNumberModelUpdate model str
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

        -- Registers table Tab
        ------------------------------------------------------------------------------------------------------------------
        CsvRequested ->
            ( model, Select.file [] CsvSelected )

        CsvSelected file ->
            ( csvSelectedModelUpdate model file
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            ( csvLoadedModelUpdate model content
            , parseModDataRequest content
            )

        ReceivedParsedModData result ->
            ( receivedParsedModDataModelUpdate model result
            , jumpToBottom "status"
            )

        UpdateModDataRequest regs ->
            ( { model | notifications = simpleNot model "Updating registers" }
            , updateModDataRequest regs
            )

        UpdateRegisters response ->
            ( readRegistersModelUpdate model response
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

        -- Heartbeat Tab
        ------------------------------------------------------------------------------------------------------------------
        -- Heartbeat
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

        StartHeartbeat ->
            startHeartbeat model

        StopHeartbeat ->
            stopHeartbeat model

        UpdateActiveHeartbeats result ->
            updateActiveHeartbeats model result

        HeartbeatChecked idx flag ->
            ( hbCheckedModelUpdate model idx flag
            , Cmd.none
            )

        InitHeartbeat result ->
            initHeartbeat model result

        HeartbeatTypeDrop opt ->
            ( hbTypeDropModelUpdate model opt, Cmd.none )

        HBLow str ->
            ( hbLowModelUpdate model str, Cmd.none )

        HBHigh str ->
            ( hbHighModelUpdate model str, Cmd.none )

        -- Settings
        ------------------------------------------------------------------------------------------------------------------
        SetActiveSetting setting ->
            ( activeSettingModelUpdate model setting
            , Cmd.none
            )

        -- Keep alive setting
        ------------------------------------------------------------------------------------------------------------------
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

        -- Word order setting
        ------------------------------------------------------------------------------------------------------------------
        ChangeWordOrderMsg settingIdx inputIdx setting ->
            ( changeWordOrderModelUpdate model settingIdx inputIdx setting
            , changeWordOrderRequest (toWordOrder setting)
            )

        ChangeWordOrderResponse result ->
            ( changeWordOrderResponseModelUpdate model result
            , jumpToBottom "status"
            )

        -- Notifications
        ------------------------------------------------------------------------------------------------------------------
        -- Expand the status bar
        ExpandStatus ->
            ( expandStatusModelUpdate model
            , Cmd.none
            )

        ExpandNotification not ->
            ( { model
                | notifications = changeNotificationState not model.notifications
              }
            , Cmd.none
            )

        -- Noop
        NoOp ->
            ( model
            , Cmd.none
            )



------------------------------------------------------------------------------------------------------------------
-- Requests & model updates
------------------------------------------------------------------------------------------------------------------
-- init command
------------------------------------------------------------------------------------------------------------------


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ getTimeZone
        , initRequest
        , initHeartbeatRequest
        ]


initRequest : Cmd Msg
initRequest =
    Http.get
        { url = "http://localhost:4000/init"
        , expect = Http.expectJson ReceivedInitInfo decodeInitInfo
        }



-- Initial request to the server to provide any running heartbeat signals
-- Nedded when the frontend is reloaded


initHeartbeatRequest : Cmd Msg
initHeartbeatRequest =
    Http.post
        { url = "http://localhost:4000/initHeartbeat"
        , body = Http.emptyBody
        , expect = Http.expectJson InitHeartbeat <| D.list decodeHeartbeat
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
            }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error retriving initial info from server"
                        (showHttpError err)
            }



-- Update the applications model based on the given ConnectionInfo


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



-- Time
------------------------------------------------------------------------------------------------------------------
-- Initialise time, will only be run on page reload
-- get server time zone


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform TimeZone Time.here


initTime : Cmd Msg
initTime =
    Task.perform InitTime Time.now



-- polls current posix time


getPosixTime : Cmd Msg
getPosixTime =
    Task.perform NewTime Time.now



-- Tab management
------------------------------------------------------------------------------------------------------------------


changeActiveTabModelUpdate : Model -> ActiveTab -> Model
changeActiveTabModelUpdate model tab =
    let
        -- retract all dropdowns on tab change
        retractedModel =
            retractDropdowns model
    in
    { retractedModel
        | activeTab = tab
    }



-- Connect tab management
------------------------------------------------------------------------------------------------------------------


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
        , expect = Http.expectWhatever ConnectionResponse
        }



-- connection response


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
              -- connection failed, connect button is active again
                | connectStatus = Connect
                , notifications =
                    detailedNot
                        model
                        "Error connecting to client"
                        (showHttpError err)
            }



-- disconnect request


disconnectRequest : Cmd Msg
disconnectRequest =
    Http.post
        { url = "http://localhost:4000/disconnect"
        , body = Http.jsonBody <| E.string "disconnect"
        , expect = Http.expectWhatever DisconnectedResponse
        }



-- disconnect response


disconnectedResponseModelUpdate : Model -> Result Http.Error () -> Model
disconnectedResponseModelUpdate model result =
    { model
      -- able to connect again
        | connectStatus = Connect
        , notifications =
            case result of
                Ok _ ->
                    simpleNot model "Disconnected"

                Err err ->
                    detailedNot
                        model
                        "Error disconnectiing from client"
                        (showHttpError err)
    }



-- TCP
------------------------------------------------------------------------------------------------------------------


changeIpAddressModelUpdate : Model -> IpAddressByte -> String -> Model
changeIpAddressModelUpdate model byte str =
    if String.isEmpty str then
        { model | ipAddress = setIpAddressByte byte model.ipAddress Nothing }

    else
        case String.toInt str |> Maybe.andThen validByte of
            Nothing ->
                model

            Just b ->
                { model | ipAddress = setIpAddressByte byte model.ipAddress (Just b) }


changePortModelUpdate : Model -> String -> Model
changePortModelUpdate model str =
    if String.isEmpty str then
        { model | socketPort = Nothing }

    else
        case String.toInt str |> Maybe.andThen validWord16 of
            Nothing ->
                model

            Just p ->
                { model | socketPort = Just p }


changeTimeoutModelUpdate : Model -> String -> Model
changeTimeoutModelUpdate model str =
    if String.isEmpty str then
        { model | timeout = Nothing }

    else
        case String.toInt str |> Maybe.andThen validWord16 of
            Nothing ->
                model

            Just t ->
                { model | timeout = Just t }



-- RTU
------------------------------------------------------------------------------------------------------------------


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



-- Register tab
------------------------------------------------------------------------------------------------------------------


regTypeDropModelUpdate : Model -> Option RegType Msg -> Model
regTypeDropModelUpdate model opt =
    { model
        | regTypeDd = setDropdown model.regTypeDd opt
        , regMdu = setRegTypeUpdate model.regMdu opt.value

        -- retract all other dropdowns on the same page
        , regModValueDd = retract model.regModValueDd
    }


regValueTypeModelUpdate : Model -> Option ModValue Msg -> Model
regValueTypeModelUpdate model opt =
    { model
        | regModValueDd = setDropdown model.regModValueDd opt
        , regMdu = setModValueUpdate model.regMdu opt.value

        -- retract all other dropdowns on the same page
        , regTypeDd = retract model.regTypeDd
    }


regAddressModelUpdate : Model -> String -> Model
regAddressModelUpdate model str =
    { model
        | regAddress = String.toInt str |> Maybe.andThen validWord16
        , regMdu = setRegAddressUpdate model.regMdu <| Maybe.withDefault 0 (String.toInt str)
    }


regUidModelUpdate : Model -> String -> Model
regUidModelUpdate model str =
    { model
        | regUid = String.toInt str |> Maybe.andThen validByte
        , regMdu = setRegUidUpdate model.regMdu <| Maybe.withDefault 1 (String.toInt str)
    }


regToggleRWModelUpdate : Model -> ReadWrite -> Model
regToggleRWModelUpdate model rw =
    if isWriteableReg model.regMdu.mduModData.modRegType then
        { model
            | regMdu = setRegRWUpdate model.regMdu rw
        }

    else
        -- reset to Read
        { model
            | regMdu = setRegRWUpdate model.regMdu Read
        }


regNumberModelUpdate : Model -> String -> Model
regNumberModelUpdate model str =
    { model | regNumReg = String.toInt str |> Maybe.andThen validWord16 }


regModValueModelUpdate : Model -> String -> Model
regModValueModelUpdate model str =
    { model | regMdu = fromModValueInputUpdate model.regMdu str }


updateRegMdu : Model -> Cmd Msg
updateRegMdu model =
    Http.post
        { url = "http://localhost:4000/modData"
        , body = Http.jsonBody <| E.list encodeModDataUpdate <| getRegMduList model.regMdu model.regNumReg
        , expect = Http.expectJson UpdateRegMduResponse <| D.list decodeModDataUpdate
        }


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



-- Registers table Tab
------------------------------------------------------------------------------------------------------------------


csvSelectedModelUpdate : Model -> File.File -> Model
csvSelectedModelUpdate model file =
    { model | csvFileName = Just <| File.name file }


csvLoadedModelUpdate : Model -> String -> Model
csvLoadedModelUpdate model content =
    { model
        | csvContent = Just content
        , csvLoaded = False
    }


parseModDataRequest : String -> Cmd Msg
parseModDataRequest content =
    Http.post
        { url = "http://localhost:4000/parseModData"
        , body = Http.jsonBody <| E.string content
        , expect = Http.expectJson ReceivedParsedModData <| D.list decodeModData
        }


receivedParsedModDataModelUpdate : Model -> Result Http.Error (List ModData) -> Model
receivedParsedModDataModelUpdate model result =
    case result of
        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error parsing register table"
                        (showHttpError err)
            }

        Ok mds ->
            { model
                | modDataUpdate = newModDataUpdate mds
                , csvLoaded = True
                , selectAllCheckbox = False
                , selectSome = False
                , notifications = simpleNot model "Register table updated"
            }


updateModDataRequest : List ModDataUpdate -> Cmd Msg
updateModDataRequest regs =
    Http.post
        { url = "http://localhost:4000/modData"
        , body = Http.jsonBody <| E.list encodeModDataUpdate regs
        , expect = Http.expectJson UpdateRegisters <| D.list decodeModDataUpdate
        }


readRegistersModelUpdate : Model -> Result Http.Error (List ModDataUpdate) -> Model
readRegistersModelUpdate model result =
    case result of
        Ok mdus ->
            { model
                | modDataUpdate = mdus
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


changeModDataValueModelUpdate : Model -> Int -> String -> Model
changeModDataValueModelUpdate model idx str =
    let
        -- get an array of the ModDataUpdate
        arrMDU =
            Array.fromList model.modDataUpdate

        -- Get a ModDataUpdate from the array and chenge the ModValue
        newMaybeMd =
            Array.get idx arrMDU
                |> Maybe.map
                    (\mdu -> { mdu | mduModData = fromModValueInput mdu.mduModData str })
    in
    case newMaybeMd of
        Nothing ->
            model

        Just md ->
            { model
                | modDataUpdate = Array.toList <| Array.set idx md arrMDU
            }



-- Heartbeat Tab
------------------------------------------------------------------------------------------------------------------


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


startHeartbeat : Model -> ( Model, Cmd Msg )
startHeartbeat model =
    let
        hbdd =
            model.hbTypeDd

        -- update selected HeartBeatType with actual low and high values
        newTypeDd =
            { hbdd | selected = updateSelectedHbType model.hbLow model.hbHigh hbdd.selected }

        mheartbeat =
            Maybe.map Heartbeat model.heartUid
                |> andMap
                    model.heartAddr
                |> andMap
                    model.heartIntv
                |> andMap
                    (Just False)
                |> andMap
                    (Just model.heartId)
                |> andMap
                    (Just newTypeDd.selected.value)
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
            , sendHeartbeats hb
            )


sendHeartbeats : Heartbeat -> Cmd Msg
sendHeartbeats hb =
    Http.post
        { url = "http://localhost:4000/startHeartbeat"
        , body = Http.jsonBody <| encodeHeartbeat <| hb
        , expect = Http.expectJson UpdateActiveHeartbeats <| D.list D.int
        }


stopHeartbeat : Model -> ( Model, Cmd Msg )
stopHeartbeat model =
    let
        ( selectedHbs, notSelectedHbs ) =
            List.partition .selected model.heartbeats
    in
    ( { model
        | heartbeats = notSelectedHbs
        , heartSelectAll = False
        , heartSelectSome = False
      }
    , stopHeartbeatRequest selectedHbs
    )


stopHeartbeatRequest : List Heartbeat -> Cmd Msg
stopHeartbeatRequest hbs =
    Http.post
        { url = "http://localhost:4000/stopHeartbeat"
        , body = Http.jsonBody <| E.list E.int <| getSelectedIds hbs
        , expect = Http.expectJson UpdateActiveHeartbeats <| D.list D.int
        }


updateActiveHeartbeats : Model -> Result Http.Error (List Int) -> ( Model, Cmd Msg )
updateActiveHeartbeats model result =
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
            if List.isEmpty diffs then
                -- All heartbeats are running
                ( { model
                    | heartbeats = hbs
                    , notifications = simpleNot model "Heartbeat signals updated"
                  }
                , jumpToBottom "status"
                )

            else
                -- Some heartbeats failed
                ( { model
                    | heartbeats = hbs
                    , notifications =
                        detailedNot model "Some heartbeat signals have failed" <|
                            List.foldl showFailedHeartbeat "Failed heartbeat signals:\n" diffs
                  }
                , jumpToBottom "status"
                )



-- updates the selected fiels in  the global heartbeat list


hbCheckedModelUpdate : Model -> Int -> Bool -> Model
hbCheckedModelUpdate model idx flag =
    let
        --
        newHB =
            List.indexedMap (replaceHeartbeatSelected idx flag) model.heartbeats
    in
    { model
        | heartbeats = newHB
        , heartSelectSome = List.any (\hb -> hb.selected) newHB
    }



-- get initial Heartbeat info from the server
-- usefull when user reloads the page


initHeartbeat : Model -> Result Http.Error (List Heartbeat) -> ( Model, Cmd Msg )
initHeartbeat model result =
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


hbTypeDropModelUpdate : Model -> Option HeartbeatType Msg -> Model
hbTypeDropModelUpdate model opt =
    { model
        | hbTypeDd =
            setDropdown
                model.hbTypeDd
                opt
    }


hbLowModelUpdate : Model -> String -> Model
hbLowModelUpdate model str =
    if String.isEmpty str then
        { model | hbLow = Nothing }

    else
        case String.toInt str |> Maybe.andThen validWord16 of
            Nothing ->
                model

            Just t ->
                { model | hbLow = Just t }


hbHighModelUpdate : Model -> String -> Model
hbHighModelUpdate model str =
    if String.isEmpty str then
        { model | hbHigh = Nothing }

    else
        case String.toInt str |> Maybe.andThen validWord16 of
            Nothing ->
                model

            Just t ->
                { model | hbHigh = Just t }



-- Settings
------------------------------------------------------------------------------------------------------------------


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



-- Keep alive setting
------------------------------------------------------------------------------------------------------------------


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



-- Send keep alive flag seperately, as the model might not update in time


keepAliveRequest : Model -> Bool -> Cmd Msg
keepAliveRequest model flag =
    case model.connectStatus of
        Connected ->
            Http.post
                { url = "http://localhost:4000/keepAlive"
                , body = Http.jsonBody <| encodeKeepAlive flag model.keepAliveIdle model.keepAliveInterval
                , expect = Http.expectJson KeepAliveResponseMsg decodeKeepAliveResponse
                }

        _ ->
            Cmd.none


keepAliveIdleModelUpdate :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> String
    -> Model
keepAliveIdleModelUpdate model settingIdx inputIdx valueStr =
    let
        checkedValue =
            String.toInt valueStr |> Maybe.andThen validWord16
    in
    case updateIndexedSetting model.settings settingIdx inputIdx (NumberInputValue checkedValue) of
        Nothing ->
            { model | keepAliveIdle = checkedValue }

        Just modifiedSettings ->
            { model
                | keepAliveIdle = checkedValue
                , settings = modifiedSettings
            }


keepAliveIntervalModelUpdate :
    Model
    -> Int -- Setting index
    -> Int -- child index
    -> String
    -> Model
keepAliveIntervalModelUpdate model settingIdx inputIdx valueStr =
    let
        checkedValue =
            String.toInt valueStr |> Maybe.andThen validWord16
    in
    case updateIndexedSetting model.settings settingIdx inputIdx (NumberInputValue checkedValue) of
        Nothing ->
            { model | keepAliveInterval = checkedValue }

        Just modifiedSettings ->
            { model
                | keepAliveInterval = checkedValue
                , settings = modifiedSettings
            }


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



-- Word order setting
------------------------------------------------------------------------------------------------------------------


changeWordOrderModelUpdate : Model -> Int -> Int -> SettingsOptions -> Model
changeWordOrderModelUpdate model settingIdx inputIdx option =
    let
        order =
            toWordOrder option
    in
    case updateIndexedSetting model.settings settingIdx inputIdx (RadioValue (Just option)) of
        Nothing ->
            model

        Just modifiedSettings ->
            { model
                | wordOrder = order
                , settings = modifiedSettings
            }


changeWordOrderRequest : WordOrder -> Cmd Msg
changeWordOrderRequest order =
    Http.post
        { url = "http://localhost:4000/wordOrder"
        , body = Http.jsonBody <| encodeWordOrder order
        , expect = Http.expectJson ChangeWordOrderResponse decodeWordOrder
        }


changeWordOrderResponseModelUpdate : Model -> Result Http.Error WordOrder -> Model
changeWordOrderResponseModelUpdate model result =
    case result of
        Ok order ->
            { model | notifications = simpleNot model (showWordOrderResponse order) }

        Err err ->
            { model
                | notifications =
                    detailedNot
                        model
                        "Error updating word order setting"
                        (showHttpError err)
            }



-- Notifications
------------------------------------------------------------------------------------------------------------------


expandStatusModelUpdate : Model -> Model
expandStatusModelUpdate model =
    case model.statusBarState of
        Expanded ->
            { model | statusBarState = Retracted }

        Retracted ->
            { model | statusBarState = Expanded }


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
-- Other
------------------------------------------------------------------------------------------------------------------


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


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)



-- haskell's <*> for elm
-- from https://github.com/circuithub/elm-maybe-extra/blob/master/src/Maybe/Extra.elm


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap x f =
    x |> Maybe.andThen (\x2 -> f |> Maybe.andThen (\f2 -> Just <| f2 x2))



-- check if Int is a valid Byte


validByte : Int -> Maybe Int
validByte int =
    if int < 0 || int > 255 then
        Nothing

    else
        Just int



-- check if Int is a valid Word16


validWord16 : Int -> Maybe Int
validWord16 int =
    if int < 0 || int > 65535 then
        Nothing

    else
        Just int
