module Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , ModValue (..)
    , ModData
    , getRegType
    , Status (..)
    )

import Http

type Msg
    = ReadRegisters (Result Http.Error (List ModData))
    | RefreshRequest (List ModData)

type alias Model =
    { modData : List ModData
    , status : Status
    }

-- See Types/ModData.hs
type RegType
    = InputRegister
    | HoldingRegister

type Status
    = AllGood
    | Loading
    | Bad String

getRegType : RegType -> String
getRegType rt =
    case rt of
        InputRegister -> "input register"
        HoldingRegister -> "holding register"

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)

type alias ModData =
    { modName : String
    , modRegType : RegType
    , modAddress : Int
    , modValue : ModValue
    , modUid : Int
    , modDescription : String
    }