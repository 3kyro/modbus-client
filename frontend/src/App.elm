module App exposing (main)

import Browser

import View exposing (view)
import Update exposing (update)
import Types exposing
    ( Msg (..)
    , Model
    , ModData
    , ModValue (..)
    , Status (..)
    , RegType (..)
    )

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
    }


initCmd : Cmd Msg
initCmd = Cmd.none

initModData : List ModData
initModData =
    [ { modName = "first"
      , modRegType = InputRegister
      , modAddress = 1
      , modValue = ModWord (Just 1)
      , modUid = 1
      , modDescription = "A register for tesing purposes"
      }
    , { modName = "second"
      , modRegType = InputRegister
      , modAddress = 2
      , modValue = ModWord (Just 2)
      , modUid = 1
      , modDescription = "A register for tesing purposes"
      }
    ]
