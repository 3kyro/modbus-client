module App exposing (main)

import Browser

import View exposing (view)
import Update exposing (update)
import Types exposing
    ( Msg (..)
    , Model
    , ModData
    , Register (..)
    , ModValue (..)
    , Status (..)
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
    , registers = initInputRegisters
    , status = AllGood
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

initInputRegisters : List Register
initInputRegisters =
    [ InputRegister (ModWord (Just 1))
    , InputRegister (ModWord (Just 2))
    , InputRegister (ModWord (Just 3))
    , InputRegister (ModWord (Just 4))
    , InputRegister (ModWord (Just 5))
    , InputRegister (ModWord (Just 6))
    ]