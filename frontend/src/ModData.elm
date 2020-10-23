module ModData exposing
    ( ModData
    , ModDataUpdate
    , ModValue(..)
    , RegType(..)
    , ValueType(..)
    , decodeModData
    , decodeModDataUpdate
    , encodeModDataUpdate
    , fromFloat
    , fromModType
    , fromModTypeUpdate
    , getModValue
    , getModValueType
    , getModValueUpdate
    , isWriteableReg
    , newModDataUpdate
    , offsetMdu
    , replaceModDataSelected
    , replaceModDataWrite
    , setRegAddressUpdate
    , setRegRWUpdate
    , setRegTypeUpdate
    , setRegUidUpdate
    , showRegType
    )

import Json.Decode as D
import Json.Encode as E
import ReadWrite
    exposing
        ( ReadWrite(..)
        , decodeRW
        , encodeRW
        )



--------------------------------------------------------------------------------------------------
-- ModData
--------------------------------------------------------------------------------------------------


type alias ModData =
    { modName : String
    , modRegType : RegType
    , modAddress : Int
    , modValue : ModValue
    , modUid : Int
    , modDescription : String
    }


encodeModData : ModData -> E.Value
encodeModData md =
    E.object
        [ ( "name", E.string md.modName )
        , ( "register type", encodeRegType md.modRegType )
        , ( "address", E.int md.modAddress )
        , ( "register value", encodeModValue md.modValue )
        , ( "uid", E.int md.modUid )
        , ( "description", E.string md.modDescription )
        ]


decodeModData : D.Decoder ModData
decodeModData =
    D.map6 ModData
        (D.field "name" D.string)
        (D.field "register type" decodeRegType)
        (D.field "address" D.int)
        (D.field "register value" decodeModValue)
        (D.field "uid" D.int)
        (D.field "description" D.string)


fromModType : ModData -> String -> ModData
fromModType md str =
    case md.modValue of
        ModWord _ ->
            { md | modValue = ModWord <| String.toInt str }

        ModFloat _ ->
            { md | modValue = ModFloat <| toMFloat str }


setRegType : ModData -> RegType -> ModData
setRegType md rt =
    { md | modRegType = rt }


setRegAddress : ModData -> Int -> ModData
setRegAddress md addr =
    { md | modAddress = addr }


setRegUid : ModData -> Int -> ModData
setRegUid md uid =
    { md | modUid = uid }


incrementModDataAddr : ModData -> ModData
incrementModDataAddr md =
    { md | modAddress = md.modAddress + getModValueMult md.modValue }


getModValueMult : ModValue -> Int
getModValueMult mv =
    case mv of
        ModWord _ ->
            1

        ModFloat _ ->
            2



--------------------------------------------------------------------------------------------------
-- ModDataUpdate
--------------------------------------------------------------------------------------------------


type alias ModDataUpdate =
    { mduModData : ModData
    , mduSelected : Bool
    , mduRW : ReadWrite
    }


newModDataUpdate : List ModData -> List ModDataUpdate
newModDataUpdate mds =
    List.map (\md -> ModDataUpdate md False Read) mds


getModValueUpdate : ModDataUpdate -> Maybe String
getModValueUpdate mdu =
    getModValue mdu.mduModData.modValue


encodeModDataUpdate : ModDataUpdate -> E.Value
encodeModDataUpdate mdu =
    E.object
        [ ( "modData", encodeModData mdu.mduModData )
        , ( "selected", E.bool <| mdu.mduSelected )
        , ( "rw", encodeRW mdu.mduRW )
        ]


decodeModDataUpdate : D.Decoder ModDataUpdate
decodeModDataUpdate =
    D.map3 ModDataUpdate
        (D.field "modData" decodeModData)
        (D.field "selected" D.bool)
        (D.field "rw" decodeRW)


replaceModDataSelected : Int -> Bool -> Int -> ModDataUpdate -> ModDataUpdate
replaceModDataSelected idx checked =
    \i md ->
        if i == idx then
            { md | mduSelected = checked }

        else
            md


replaceModDataWrite : Int -> ReadWrite -> Int -> ModDataUpdate -> ModDataUpdate
replaceModDataWrite idx rw =
    \i md ->
        if i == idx && isWriteableReg md.mduModData.modRegType then
            { md | mduRW = rw }

        else
            md


fromModTypeUpdate : ModDataUpdate -> String -> ModDataUpdate
fromModTypeUpdate mdu str =
    { mdu | mduModData = fromModType mdu.mduModData str }


setRegTypeUpdate : ModDataUpdate -> RegType -> ModDataUpdate
setRegTypeUpdate mdu rt =
    if isWriteableReg rt then
        { mdu | mduModData = setRegType mdu.mduModData rt }

    else
        -- Only writeable register can be Write
        { mdu
            | mduModData = setRegType mdu.mduModData rt
            , mduRW = Read
        }


setRegAddressUpdate : ModDataUpdate -> Int -> ModDataUpdate
setRegAddressUpdate mdu addr =
    { mdu | mduModData = setRegAddress mdu.mduModData addr }


setRegUidUpdate : ModDataUpdate -> Int -> ModDataUpdate
setRegUidUpdate mdu uid =
    { mdu | mduModData = setRegUid mdu.mduModData uid }


setRegRWUpdate : ModDataUpdate -> ReadWrite -> ModDataUpdate
setRegRWUpdate mdu rw =
    { mdu | mduRW = rw }


offsetMdu : ModDataUpdate -> Int -> List ModDataUpdate
offsetMdu mdu num =
    let
        mdus =
            List.repeat (num - 1) mdu
    in
    mdu
        :: List.map
            (\m -> { m | mduModData = incrementModDataAddr m.mduModData })
            mdus



--------------------------------------------------------------------------------------------------
-- RegType
--------------------------------------------------------------------------------------------------


type RegType
    = InputRegister
    | HoldingRegister


isWriteableReg : RegType -> Bool
isWriteableReg rt =
    case rt of
        InputRegister ->
            False

        HoldingRegister ->
            True


showRegType : RegType -> String
showRegType rt =
    case rt of
        InputRegister ->
            "Input Register"

        HoldingRegister ->
            "Holding Register"


encodeRegType : RegType -> E.Value
encodeRegType rt =
    case rt of
        InputRegister ->
            E.string "input register"

        HoldingRegister ->
            E.string "holding register"


decodeRegType : D.Decoder RegType
decodeRegType =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "input register" ->
                        D.succeed InputRegister

                    "holding register" ->
                        D.succeed HoldingRegister

                    _ ->
                        D.fail "Not a Register Type"
            )



--------------------------------------------------------------------------------------------------
-- ValueType
--------------------------------------------------------------------------------------------------


type ValueType
    = VWord
    | VFloat



--------------------------------------------------------------------------------------------------
-- ModValue
--------------------------------------------------------------------------------------------------


type ModValue
    = ModWord (Maybe Int)
    | ModFloat (Maybe MFloat)


getModValueType : ModValue -> String
getModValueType mv =
    case mv of
        ModWord _ ->
            "Word"

        ModFloat _ ->
            "Float"


getModValue : ModValue -> Maybe String
getModValue mv =
    case mv of
        ModWord v ->
            Maybe.map String.fromInt v

        ModFloat v ->
            Maybe.map showMFloat v


encodeModValue : ModValue -> E.Value
encodeModValue mv =
    case mv of
        ModWord (Just x) ->
            E.object
                [ ( "type", E.string "word" )
                , ( "value", E.int x )
                ]

        ModWord Nothing ->
            E.object
                [ ( "type", E.string "word" )
                ]

        ModFloat (Just x) ->
            E.object
                [ ( "type", E.string "float" )
                , ( "value", E.float x.flt )
                ]

        ModFloat Nothing ->
            E.object
                [ ( "type", E.string "float" )
                ]


decodeModValue : D.Decoder ModValue
decodeModValue =
    D.field "type" D.string
        |> D.andThen
            (\s ->
                case s of
                    "word" ->
                        D.map ModWord <| D.field "value" (D.nullable D.int)

                    "float" ->
                        D.map ModFloat <| D.field "value" (D.nullable decodeMFloat)

                    _ ->
                        D.fail "Not a valid ModValue"
            )



--------------------------------------------------------------------------------------------------
-- MFloat
--------------------------------------------------------------------------------------------------
-- Custom type to overcome a limitaion of elm when updating float inputs
-- Speciffically "1." is a valid float that is shown as "1"
-- This blocks inputs after a dot is typed


type alias MFloat =
    { str : String
    , flt : Float
    }



-- always show the string, not the float


showMFloat : MFloat -> String
showMFloat mf =
    mf.str



-- save the string in case of a valid parse


toMFloat : String -> Maybe MFloat
toMFloat s =
    Maybe.map (MFloat s) <| String.toFloat s


fromFloat : Float -> MFloat
fromFloat f =
    MFloat (String.fromFloat f) f


decodeMFloat : D.Decoder MFloat
decodeMFloat =
    D.map fromFloat D.float
