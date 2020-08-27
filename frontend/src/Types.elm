module Types exposing
    ( Msg
    , Model
    , Register (..)
    , ModValue (..)
    , ModData
    , showRegister
    , getValue
    )

type alias Msg
    = ModData

type alias Model =
    { modData : List ModData
    , inputRegisters : List Register
    , holdingRegisters : List Register
    }

-- See Types/ModData.hs
type Register
    = InputRegister ModValue
    | HoldingRegister ModValue


showRegister : Register -> String
showRegister rt =
    case rt of
        InputRegister _ -> "Input Register"
        HoldingRegister _ -> "Holding Register"

getValue : Register -> ModValue
getValue reg =
    case reg of
        InputRegister v -> v
        HoldingRegister v -> v

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)

type alias ModData =
    { name : String
    , register : Register
    , address : Int
    , description : String
    }