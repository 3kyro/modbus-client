module App exposing (main)

import Browser

import View exposing (view)
import Update exposing (update, initCmd)
import Types exposing
    ( Msg (..)
    , Model
    , ModData
    , ModValue (..)
    , Status (..)
    , StatusBarState(..)
    , RegType (..)
    , ConnectStatus(..)
    , ActiveTab(..)
    , ReadWrite(..)
    , newModDataUpdate
    , fromFloat
    )
import Types.IpAddress exposing (defaultIpAddr)

main : Program () Model Msg
main = Browser.element
    { init = \_ -> (initModel, initCmd)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

initModel : Model
initModel =
    { modDataUpdate = newModDataUpdate initModData
    , status = AllGood
    , statusBarState = Retracted
    , connectStatus = Connect
    , ipAddress = defaultIpAddr
    , socketPort = Just 502
    , timeout = Just 1000
    , activeTab = ConnectMenu
    , csvFileName = Nothing
    , csvContent = Nothing
    , csvLoaded = False
    , selectAllCheckbox = False
    , selectSome = False
    , readWriteAll = Read
    }

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
