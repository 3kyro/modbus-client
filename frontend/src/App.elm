module App exposing (main)

import Browser
import Dropdown exposing (Dropdown, Option, getOption)
import Element exposing (text)
import Element.Input exposing (checkbox)
import Notifications exposing (StatusBarState(..))
import Settings exposing (Setting, SettingInput(..), SettingStatus(..))
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ByteOrder(..)
        , ConnectStatus(..)
        , DummyOption(..)
        , ModData
        , ModValue(..)
        , Model
        , Msg(..)
        , ReadWrite(..)
        , RegType(..)
        , SettingOption(..)
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
    , dummyMessageStatus = False
    , dummyDropdown = dummyDrop
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


dummyDrop : Dropdown DummyOption Msg
dummyDrop =
    { onClick = ExpandDummyDropdown
    , options = [ dummyOption1, dummyOption2, dummyOption3, dummyOption4 ]
    , selected = dummyOption1
    , expanded = False
    , label = "Dummy label"
    }



-- newRegisterTab [] []


dummyOption1 : Option DummyOption Msg
dummyOption1 =
    getOption DummyOption1 (text "Dummy option 1")


dummyOption2 : Option DummyOption Msg
dummyOption2 =
    getOption DummyOption2 (text "Dummy option 2")


dummyOption3 : Option DummyOption Msg
dummyOption3 =
    getOption DummyOption3 (text "Dummy option 3")


dummyOption4 : Option DummyOption Msg
dummyOption4 =
    getOption DummyOption4 (text "Dummy option 4")


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
