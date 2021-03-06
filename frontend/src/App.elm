module App exposing (main)

import Browser
import Dropdown exposing (Dropdown, Option, getOption)
import Element exposing (text)
import Element.Input exposing (checkbox)
import ModData
    exposing
        ( ModData
        , ModDataUpdate
        , ModValue(..)
        , RegType(..)
        , fromFloat
        , newModDataUpdate
        )
import Notifications exposing (StatusBarState(..))
import ReadWrite exposing (ReadWrite(..))
import Settings exposing (Setting, SettingInput(..), SettingStatus(..))
import Time
import Types
    exposing
        ( ActiveTab(..)
        , BaudRate(..)
        , ConnectActiveTab(..)
        , ConnectStatus(..)
        , Heartbeat
        , HeartbeatType(..)
        , Model
        , Msg(..)
        , OS(..)
        , Parity(..)
        , SettingsOptions(..)
        , StopBits(..)
        , WordOrder(..)
        )
import Types.IpAddress exposing (defaultIpAddr)
import Update exposing (initCmd, update)
import View exposing (view)
import Heartbeat exposing (hbTypeDropDown)

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
    { -- register table
      modDataUpdate = []
    , selectAllCheckbox = False
    , selectSome = False
    , readWriteAll = Read

    -- register tab
    , regTypeDd = regDropdown
    , regModValueDd = valueTypeDropdown
    , regAddress = Just 0
    , regUid = Just 1
    , regNumReg = Nothing
    , regMdu = initRegMdu
    , regResponse = []

    -- notifications
    , statusBarState = Retracted
    , notifications = []
    , connectStatus = Connect

    -- tabs
    , activeTab = ConnectMenu

    -- connect tab
    , connActiveTab = TCPTab
    , timeout = Just 10

    -- TCP connections
    , ipAddress = defaultIpAddr
    , socketPort = Just 502

    -- RTU Connections
    , serialPort = Nothing
    , os = Linux
    , baudrate = BR9600
    , baudrateDd = baudrateDd
    , stopBits = OneStopBit
    , stopBitsDd = stopbitsDd
    , parity = OddParity
    , parityDd = parityDd

    -- csv
    , csvFileName = Nothing
    , csvContent = Nothing
    , csvLoaded = False

    -- time
    , timePosix = Time.millisToPosix 0
    , timeZone = Time.utc

    -- heartbeats
    , heartbeats = []
    , heartUid = Nothing
    , heartAddr = Nothing
    , heartIntv = Nothing
    , heartSelectAll = False
    , heartSelectSome = False
    , hbTypeDd = hbTypeDropDown 0 0 False
    , heartId = 0
    , hbLow = Nothing
    , hbHigh = Nothing

    -- settings
    , settings = [ keepAliveSetting, wordOrderSetting ]
    , keepAlive = False
    , keepAliveIdle = Nothing
    , keepAliveInterval = Nothing
    , wordOrder = LE

    -- version
    , version = "0.2.1"
    }



----------------------------------------------------------------------------------------------------------------------------------
-- Register dropdowns
----------------------------------------------------------------------------------------------------------------------------------


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
    getOption InputRegister (text "Input")


holdingRegisterOption : Option RegType Msg
holdingRegisterOption =
    getOption HoldingRegister (text "Holding")


valueTypeDropdown : Dropdown ModValue Msg
valueTypeDropdown =
    { onClick = RegValueTypeDrop
    , options = [ wordOption, bitsOption, floatOption, doubleOption ]
    , selected = wordOption
    , expanded = False
    , label = ""
    }


wordOption : Option ModValue Msg
wordOption =
    getOption (ModWord Nothing) (text "Word")


bitsOption : Option ModValue Msg
bitsOption =
    getOption (ModBits Nothing) (text "Bits")


floatOption : Option ModValue Msg
floatOption =
    getOption (ModFloat Nothing) (text "Float")

doubleOption : Option ModValue Msg
doubleOption =
    getOption (ModDouble Nothing) (text "Double")


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
    , mduSelected = True
    , mduRW = Read
    }



----------------------------------------------------------------------------------------------------------------------------------
-- Connection Dropdowns
----------------------------------------------------------------------------------------------------------------------------------


baudrateDd : Dropdown BaudRate Msg
baudrateDd =
    { onClick = BaudRateDrop
    , options = baudrateOptions
    , selected = getOption BR9600 (text "9600")
    , expanded = False
    , label = ""
    }


baudrateOptions : List (Option BaudRate Msg)
baudrateOptions =
    [ getOption BR110 (text "110")
    , getOption BR300 (text "300")
    , getOption BR600 (text "600")
    , getOption BR1200 (text "1200")
    , getOption BR2400 (text "2400")
    , getOption BR4800 (text "4800")
    , getOption BR9600 (text "9600")
    , getOption BR19200 (text "19200")
    , getOption BR38400 (text "38400")
    , getOption BR57600 (text "57600")
    , getOption BR115200 (text "115200")
    ]


stopbitsDd : Dropdown StopBits Msg
stopbitsDd =
    { onClick = StopBitsDrop
    , options = [ oneSBOpt, twoSBOpt ]
    , selected = oneSBOpt
    , expanded = False
    , label = ""
    }


oneSBOpt : Option StopBits Msg
oneSBOpt =
    getOption OneStopBit (text "One")


twoSBOpt : Option StopBits Msg
twoSBOpt =
    getOption TwoStopBits (text "Two")


parityDd : Dropdown Parity Msg
parityDd =
    { onClick = ParityDrop
    , options = [ odd, even, noParity ]
    , selected = odd
    , expanded = False
    , label = ""
    }


odd : Option Parity Msg
odd =
    getOption OddParity (text "Odd")


even : Option Parity Msg
even =
    getOption EvenParity (text "Even")

noParity : Option Parity Msg
noParity =
    getOption NoParity (text "No Parity")







----------------------------------------------------------------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------------------------------------------------------------


keepAliveSetting : Setting SettingsOptions Msg
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


wordOrderSetting : Setting SettingsOptions Msg
wordOrderSetting =
    Setting
        "Word order: Defines the order of sequence of individual words in multi-word data types (e.g. Float)"
        NotActive
        [ Radio
            { description = "Order"
            , values =
                [ ( SetLE, "Little Endian" )
                , ( SetBE, "Big Endian" )
                ]
            , selected = Just SetLE
            , message = ChangeWordOrderMsg
            }
        ]
