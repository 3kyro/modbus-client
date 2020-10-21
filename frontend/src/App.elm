module App exposing (main)

import Browser
import Dropdown exposing (Dropdown, Option, getOption)
import Element exposing (text)
import Element.Input exposing (checkbox)
import Notifications exposing (StatusBarState(..))
import ReadWrite exposing (ReadWrite(..))
import Settings exposing (Setting, SettingInput(..), SettingStatus(..))
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ByteOrder(..)
        , ConnectStatus(..)
        , ModData
        , ModDataUpdate
        , ModValue(..)
        , Model
        , Msg(..)
        , RegType(..)
        , SettingOption(..)
        , ValueType(..)
        , fromFloat
        , newModDataUpdate
        )
import Types.IpAddress exposing (defaultIpAddr)
import Update exposing (initCmd, update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, initCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Time.every 1000 NewTime
        }


initModel : Model
initModel =
    { modDataUpdate = newModDataUpdate initModData
    , notifications = []
    , statusBarState = Retracted
    , connectStatus = Connect
    , ipAddress = defaultIpAddr
    , socketPort = Just 502
    , serialPort = Nothing
    , timeout = Just 60
    , byteOrder = LE
    , activeTab = ConnectMenu
    , csvFileName = Nothing
    , csvContent = Nothing
    , csvLoaded = False
    , selectAllCheckbox = False
    , selectSome = False
    , readWriteAll = Read
    , timePosix = Time.millisToPosix 0
    , timeZone = Time.utc
    , settings = [ keepAliveSetting, byteOrderSetting ]
    , keepAlive = False
    , keepAliveIdle = Nothing
    , keepAliveInterval = Nothing
    , regTypeDd = regDropdown
    , valueTypeDd = valueTypeDropdown
    , regAddress = Just 0
    , regUid = Just 1
    , regNumReg = Nothing
    , regMdu = initRegMdu
    }


regDropdown : Dropdown RegType Msg
regDropdown =
    { onClick = RegRegTypeDrop
    , options = [ inputRegisterOption, holdingRegisterOption ]
    , selected = inputRegisterOption
    , expanded = False
    , label = ""
    }


getRegTypeOption : RegType -> Option RegType Msg
getRegTypeOption rg =
    case rg of
        InputRegister ->
            inputRegisterOption

        HoldingRegister ->
            holdingRegisterOption


inputRegisterOption : Option RegType Msg
inputRegisterOption =
    getOption InputRegister (text "Input Register")


holdingRegisterOption : Option RegType Msg
holdingRegisterOption =
    getOption HoldingRegister (text "Holding Register")


valueTypeDropdown : Dropdown ValueType Msg
valueTypeDropdown =
    { onClick = RegValueTypeDrop
    , options = [ wordOption, floatOption ]
    , selected = wordOption
    , expanded = False
    , label = ""
    }


wordOption : Option ValueType Msg
wordOption =
    getOption VWord (text "Word")


floatOption : Option ValueType Msg
floatOption =
    getOption VFloat (text "Float")


initRegMdu : ModDataUpdate
initRegMdu =
    { mduModData =
        { modName = "Register"
        , modRegType = InputRegister
        , modAddress = 0
        , modValue = ModWord Nothing
        , modUid = 1
        , modDescription = "Used for getting raw registers from the server"
        }
    , mduSelected = False -- not used in this context
    , mduRW = Read
    }


keepAliveSetting : Setting SettingOption Msg
keepAliveSetting =
    Setting
        "Keep Alive"
        NotActive
        [ CheckBox
            { description = "Enable keep alive"
            , flag = False
            , message = KeepAliveMsg
            }
        , NumberInput
            { description = "Keep alive idle (s)"
            , value = Just 60
            , message = KeepAliveIdleMsg
            }
        , NumberInput
            { description = "Keep alive interval (s)"
            , value = Just 10
            , message = KeepAliveIntervalMsg
            }
        ]


byteOrderSetting : Setting SettingOption Msg
byteOrderSetting =
    Setting
        "Byte order"
        NotActive
        [ Radio
            { description = "Endianess"
            , values =
                [ ( SetLE, "Little Endian" )
                , ( SetBE, "Big Endian" )
                ]
            , selected = Just SetLE
            , message = ChangeByteOrderMsg
            }
        ]


initModData : List ModData
initModData =
    [ { modName = "first"
      , modRegType = HoldingRegister
      , modAddress = 1
      , modValue = ModFloat (Just <| fromFloat 1)
      , modUid = 1
      , modDescription = "A register for testing purposes"
      }
    , { modName = "second"
      , modRegType = HoldingRegister
      , modAddress = 2
      , modValue = ModWord (Just 2)
      , modUid = 1
      , modDescription = "A register for testing purposes"
      }
    , { modName = "1500"
      , modRegType = InputRegister
      , modAddress = 10
      , modValue = ModWord Nothing
      , modUid = 1
      , modDescription = "A register for testing purposes"
      }
    , { modName = "1700"
      , modRegType = HoldingRegister
      , modAddress = 15
      , modValue = ModWord Nothing
      , modUid = 1
      , modDescription = "A register for testing purposes"
      }
    ]
