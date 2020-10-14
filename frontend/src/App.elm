module App exposing (main)

import Browser
import Notifications exposing (StatusBarState(..))
import Time
import Types
    exposing
        ( ActiveTab(..)
        , ConnectStatus(..)
        , ModData
        , ModValue(..)
        , Model
        , Msg(..)
        , ReadWrite(..)
        , RegType(..)
        , fromFloat
        , newModDataUpdate
        )
import Types.IpAddress exposing (defaultIpAddr)
import Update exposing (initCmd, update)
import View exposing (view)
import Settings exposing (Setting , SettingStatus (..), SettingInput (..), dummySetting)
import Element.Input exposing (checkbox)

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
    , timeout = Just 1000
    , activeTab = ConnectMenu
    , csvFileName = Nothing
    , csvContent = Nothing
    , csvLoaded = False
    , selectAllCheckbox = False
    , selectSome = False
    , readWriteAll = Read
    , timePosix = Time.millisToPosix 0
    , timeZone = Time.utc
    , settings = [anotherSetting, dummySetting, { dummySetting | description = "lalal"}]
    , keepAlive = False
    }

anotherSetting : Setting Msg
anotherSetting =
    Setting
        "A checkbox setting"
        Active
        [ CheckBox
            { description = "Checkbox description"
            , flag = False
            , message = DummyCheckboxMsg
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
