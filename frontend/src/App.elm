module App exposing (main)

import Browser

import Types exposing (..)
import View exposing (view)
import Update exposing (update)

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
    , inputRegisters = []
    , holdingRegisters = []
    }


initCmd : Cmd Msg
initCmd = Cmd.none

initModData : List ModData
initModData =
    [
    { name = "test"
    , register = InputRegister (ModWord (Just 1))
    , address = 3000
    , description = "A register for tesing purposes"
    }
    ]