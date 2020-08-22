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
    { name = "test"
    , regType = InputRegister
    , address = 3000
    , value = ModWord (Just 1)
    , description = "A register for tesing purposes"
    }

initCmd : Cmd Msg
initCmd = Cmd.none