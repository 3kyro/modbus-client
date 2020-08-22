module Types exposing
    ( Msg
    , Model
    , RegType (..)
    , ModValue (..)
    , ModData
    , showRegType
    )

type alias Msg
    = ModData

type alias Model = ModData

-- See Types/ModData.hs
type RegType
    = DiscreteInput
    | Coil
    | InputRegister
    | HoldingRegister


showRegType : RegType -> String
showRegType rt =
    case rt of
        DiscreteInput -> "Discrete Input"
        Coil -> "Coil"
        InputRegister -> "Input Register"
        HoldingRegister -> "Holding Register"

type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe Float)

type alias ModData =
    { name : String
    , regType : RegType
    , address : Int
    , value : ModValue
    , description : String
    }