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
    , RegType (..)
    , ConnectStatus(..)
    , ActiveMenu(..)
    , ActiveTable(..)
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
    { modData = initModData
    , status = AllGood
    , connectStatus = Connect
    , ipAddress = defaultIpAddr
    , socketPort = Just 502
    , timeout = Just 1000
    , activeMenu = NoneActive
    , activeTable = ModDataTable
    , csvFileName = Nothing
    , csvContent = Nothing
    , selectAllCheckbox = False
    }

initModData : List ModData
initModData =
    [ { modName = "first"
      , modRegType = InputRegister
      , modAddress = 1
      , modValue = ModWord (Just 1)
      , modUid = 1
      , modDescription = "A register for tesing purposes"
      , selected = False
      }
    , { modName = "second"
      , modRegType = InputRegister
      , modAddress = 2
      , modValue = ModWord (Just 2)
      , modUid = 1
      , modDescription = "A register for tesing purposes"
      , selected = False
      }
    , { modName = "1500"
      , modRegType = InputRegister
      , modAddress = 10
      , modValue = ModWord Nothing
      , modUid = 1
      , modDescription = "A register for tesing purposes"
      , selected = False
      }
    , { modName = "1700"
      , modRegType = HoldingRegister
      , modAddress = 15
      , modValue = ModWord Nothing
      , modUid = 1
      , modDescription = "A register for tesing purposes"
      , selected = False
      }
    ]
