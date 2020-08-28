module Types exposing
    ( Msg (..)
    , Model
    , Register (..)
    , ModValue (..)
    , ModData
    , getRegType
    , getRegValue
    , showRegValueType
    , Status (..)
    )

import Http

type Msg
    = ReadRegisters (Result Http.Error (List Register))
    | RefreshRequest (List Register)

type alias Model =
    { modData : List ModData
    , registers : List Register
    , status : Status
    }

-- See Types/ModData.hs
type Register
    = InputRegister ModValue
    | HoldingRegister ModValue

type Status
    = AllGood
    | Loading
    | Bad String

getRegType : Register -> String
getRegType rt =
    case rt of
        InputRegister _ -> "Input Register"
        HoldingRegister _ -> "Holding Register"

getRegValue : Register -> ModValue
getRegValue reg =
    case reg of
        InputRegister v -> v
        HoldingRegister v -> v

showRegValueType : Register -> String
showRegValueType reg =
    case getRegValue reg of
        ModWord _ -> "word"
        ModFloat _ -> "float"

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)

type alias ModData =
    { name : String
    , register : Register
    , address : Int
    , description : String
    }