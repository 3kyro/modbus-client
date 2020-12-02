module ModData exposing
    ( ModData
    , ModDataUpdate
    , ModValue(..)
    , RegType(..), getRegMduList, modEmptyColumn, readWriteColumn
    , bitsFromString
    , decodeModData
    , decodeModDataUpdate
    , encodeModDataUpdate
    , fromFloat
    , fromModValueInput
    , fromModValueInputUpdate
    , fuzzModData
    , getModValue
    , showModValueType
    , getModValueUpdate
    , isWriteableReg
    , modAddressColumn
    , modDescriptionColumn
    , modNameColumn
    , modRegTypeColumn
    , modUidColumn
    , modValueColumn
    , modValueTypeColumn
    , newModDataUpdate
    , offsetMdu
    , replaceModDataSelected
    , replaceModDataWrite
    , selectColumn
    , setModValueUpdate
    , setRegAddressUpdate
    , setRegRWUpdate
    , setRegTypeUpdate
    , setRegUidUpdate
    , showRegType
    , tableCellColor
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Fuzz as Fuzz exposing (Fuzzer)
import Json.Decode as D
import Json.Encode as E
import Palette
    exposing
        ( grey
        , greyWhite
        , lightGrey
        , blueSapphire
        , fireBrick
        )
import ReadWrite
    exposing
        ( ReadWrite(..)
        , decodeRW
        , encodeRW
        , readWriteButton
        , flipRW
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


fuzzModData : Fuzzer ModData
fuzzModData =
    Fuzz.map ModData Fuzz.string
        |> Fuzz.andMap regTypeFuzz
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap modValueFuzzer
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.string


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


fromModValueInput : ModData -> String -> ModData
fromModValueInput md str =
    case md.modValue of
        ModWord _ ->
            { md | modValue = ModWord <| String.toInt str }

        ModBits _ ->
            if bitsValidString str then
                { md
                    | modValue =
                        ModBits <|
                            bitsFromString str
                }

            else
                md

        ModFloat _ ->
            { md | modValue = ModFloat <| toMFloat str }

        ModDouble _ ->
            { md | modValue = ModDouble <| toMFloat str }


setRegType : ModData -> RegType -> ModData
setRegType md rt =
    { md | modRegType = rt }


setRegAddress : ModData -> Int -> ModData
setRegAddress md addr =
    { md | modAddress = addr }


setRegUid : ModData -> Int -> ModData
setRegUid md uid =
    { md | modUid = uid }


setModValue : ModData -> ModValue -> ModData
setModValue md mv =
    { md | modValue = mv }


incrementModDataAddr : ModData -> Int -> ModData
incrementModDataAddr md i =
    { md | modAddress = md.modAddress + i * getModValueMult md.modValue }


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


fromModValueInputUpdate : ModDataUpdate -> String -> ModDataUpdate
fromModValueInputUpdate mdu str =
    { mdu | mduModData = fromModValueInput mdu.mduModData str }


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


setModValueUpdate : ModDataUpdate -> ModValue -> ModDataUpdate
setModValueUpdate mdu mv =
    { mdu | mduModData = setModValue mdu.mduModData mv }


-- create a list of ModDataUpdates
getRegMduList : ModDataUpdate -> Maybe Int -> List ModDataUpdate
getRegMduList mdu mnum =
    case mdu.mduRW of
        Write ->
            -- only write a single register
            [ mdu ]

        Read ->
            case mnum of
                Nothing ->
                    []

                -- offset the given mdu based on the requested number of registers
                Just num ->
                    offsetMdu mdu num

-- crete a list of ModDataUpdate by offeseting a base mdu by a given number
offsetMdu : ModDataUpdate -> Int -> List ModDataUpdate
offsetMdu mdu num =
    let
        mdus =
            List.repeat num mdu
    in
    -- increment ModData address
    List.indexedMap
        (\i m -> { m | mduModData = incrementModDataAddr m.mduModData i })
        mdus




----------------------------------------------------------------------------------------------------------------------------------
-- View ModDataUpdate
----------------------------------------------------------------------------------------------------------------------------------
-- Column for selection checkboxes


selectColumn : (Bool -> msg) -> Bool -> (Int -> Bool -> msg) -> IndexedColumn ModDataUpdate msg
selectColumn selectAllmsg flag selectOnemsg =
    { header =
        el
            [ height <| px 38
            , paddingXY 10 0
            ]
        <|
            selectCheckbox selectAllmsg flag
    , width = fillPortion 1 |> maximum 30
    , view = \i md -> viewCheckedCell selectOnemsg i md.mduSelected
    }



-- A generic checkbox


selectCheckbox : (Bool -> msg) -> Bool -> Element msg
selectCheckbox msg flag =
    Input.checkbox
        [ alignLeft
        , centerY
        ]
        { onChange = msg
        , icon = Input.defaultCheckbox
        , checked = flag
        , label = Input.labelHidden "Select Checkbox"
        }



-- A checkbox inside a table cell


viewCheckedCell : (Int -> Bool -> msg) -> Int -> Bool -> Element msg
viewCheckedCell msg idx selected =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        , paddingXY 10 0
        ]
    <|
        selectCheckbox (msg idx) selected


readWriteColumn : (ReadWrite -> msg) -> ReadWrite -> (Int -> ReadWrite -> msg) -> IndexedColumn ModDataUpdate msg
readWriteColumn rwAllmsg readwriteall rwOnemsg =
    { header =
        el
            [ height <| px 38
            , Font.color greyWhite
            , centerX
            , width <| fillPortion 1
            ]
        <|
            readWriteButton
                readwriteall
                blueSapphire
                fireBrick
            <|
                Just <|
                    rwAllmsg <|
                        flipRW readwriteall
    , width = px 50
    , view = \i md -> viewReadWriteModDataCell i md rwOnemsg
    }

viewReadWriteModDataCell : Int -> ModDataUpdate -> (Int -> ReadWrite -> msg) -> Element msg
viewReadWriteModDataCell idx md msg =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
    <|
        if isWriteableReg md.mduModData.modRegType then
            readWriteButton md.mduRW
                blueSapphire
                fireBrick
            <|
                Just <|
                    msg idx <|
                        flipRW md.mduRW

        else
            none

modNameColumn : IndexedColumn ModDataUpdate msg
modNameColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Name"
    , width = fill |> minimum 200 
    , view = \i md -> viewCell i md.mduModData.modName
    }


modRegTypeColumn : IndexedColumn ModDataUpdate msg
modRegTypeColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Type"
    , width = fill |> minimum 100 |> maximum 200
    , view = \i md -> viewCell i <| showRegType md.mduModData.modRegType
    }


modAddressColumn : IndexedColumn ModDataUpdate msg
modAddressColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Address"
    , width = fill |> minimum 100 |> maximum 200
    , view = \i md -> viewCell i <| String.fromInt md.mduModData.modAddress
    }


modValueTypeColumn : IndexedColumn ModDataUpdate msg
modValueTypeColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Value Type"
    , width = fill |> minimum 100 |> maximum 200
    , view = \i md -> viewCell i <| showModValueType md.mduModData.modValue
    }


modValueColumn : Maybe (Int -> String -> msg) -> IndexedColumn ModDataUpdate msg
modValueColumn cmd =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Value"
    , width = fill |> minimum 150 |> maximum 200
    , view = \idx md -> viewModValueColumn cmd idx md
    }


viewModValueColumn : Maybe (Int -> String -> msg) -> Int -> ModDataUpdate -> Element msg
viewModValueColumn cmd idx md =
    case md.mduRW of
        Read ->
            viewReadModValue idx md.mduModData

        Write ->
            viewWriteModValue cmd idx md.mduModData


viewWriteModValue : Maybe (Int -> String -> msg) -> Int -> ModData -> Element msg
viewWriteModValue mcmd idx md =
    case mcmd of
        Nothing ->
            viewReadModValue idx md

        Just cmd ->
            el
                [ Background.color <| tableCellColor idx
                , Font.center
                ]
            <|
                Input.text
                    [ Background.color <| tableCellColor idx
                    , Font.color greyWhite
                    , Border.width 1
                    , height <| px 38
                    , width fill

                    -- 11 is a magic number here :(
                    , paddingXY 0 11
                    , focused []
                    ]
                    { onChange = cmd idx
                    , text = Maybe.withDefault "" <| getModValue md.modValue
                    , placeholder = Nothing
                    , label = Input.labelHidden "Value Input"
                    }


viewReadModValue : Int -> ModData -> Element msg
viewReadModValue idx md =
    viewCell idx <|
        Maybe.withDefault "Nothing" <|
            getModValue md.modValue


modUidColumn : IndexedColumn ModDataUpdate msg
modUidColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Unit Id"
    , width = fill |> minimum 80 |> maximum 100
    , view = \i md -> viewCell i <| String.fromInt md.mduModData.modUid
    }


modDescriptionColumn : IndexedColumn ModDataUpdate msg
modDescriptionColumn =
    { header = el [ height <| px 38 ] <| el [ alignLeft, centerY ] <| text "Description"
    , width = fill
    , view = \i md -> viewDescCell i md.mduModData.modDescription
    }


viewDescCell : Int -> String -> Element msg
viewDescCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
        (el [ alignLeft, centerY ] <| text str)

-- Hack to fill empty space in a ModDataYpdate table
modEmptyColumn : IndexedColumn ModDataUpdate msg
modEmptyColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text ""
    , width = fill
    , view = \i md -> viewEmptyColumn i
    }

viewEmptyColumn : Int -> Element msg
viewEmptyColumn idx =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
        (el [ alignLeft, centerY ] <| text " ")


headerTextAttr : List (Attribute msg)
headerTextAttr =
    [ centerX, centerY, paddingXY 10 0 ]


viewCell : Int -> String -> Element msg
viewCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        , width fill
        , paddingXY 10 0
        ]
        (el [ centerX, centerY ] <| text str)


tableCellColor : Int -> Color
tableCellColor idx =
    if modBy 2 idx == 0 then
        lightGrey

    else
        grey



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


regTypeFuzz : Fuzzer RegType
regTypeFuzz =
    Fuzz.oneOf
        [ Fuzz.constant InputRegister
        , Fuzz.constant HoldingRegister
        ]



--------------------------------------------------------------------------------------------------
-- ModValue
--------------------------------------------------------------------------------------------------


type ModValue
    = ModWord (Maybe Int)
    | ModBits (Maybe Bits)
    | ModFloat (Maybe MFloat)
    | ModDouble (Maybe MFloat)


showModValueType : ModValue -> String
showModValueType mv =
    case mv of
        ModWord _ ->
            "Word"

        ModBits _ ->
            "Word Bits"

        ModFloat _ ->
            "Float"

        ModDouble _ ->
            "Double"


getModValue : ModValue -> Maybe String
getModValue mv =
    case mv of
        ModWord v ->
            Maybe.map String.fromInt v

        ModBits v ->
            Maybe.map .value v

        ModFloat v ->
            Maybe.map showMFloat v

        ModDouble v ->
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

        ModBits (Just x) ->
            E.object
                [ ( "type", E.string "bits" )
                , ( "value", E.string x.value )
                ]

        ModBits Nothing ->
            E.object
                [ ( "type", E.string "bits" )
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

        ModDouble (Just x) ->
            E.object
                [ ( "type", E.string "double" )
                , ( "value", E.float x.flt )
                ]

        ModDouble Nothing ->
            E.object
                [ ( "type", E.string "double" )
                ]


decodeModValue : D.Decoder ModValue
decodeModValue =
    D.field "type" D.string
        |> D.andThen
            (\s ->
                case s of
                    "word" ->
                        D.map ModWord <| D.field "value" (D.nullable D.int)

                    "bits" ->
                        D.map ModBits <|
                            D.field "value" <|
                                D.nullable decodeBits

                    "float" ->
                        D.map ModFloat <| D.field "value" (D.nullable decodeMFloat)

                    "double" ->
                        D.map ModDouble <| D.field "value" (D.nullable decodeMFloat)

                    _ ->
                        D.fail "Not a valid ModValue"
            )


getModValueMult : ModValue -> Int
getModValueMult mv =
    case mv of
        ModWord _ ->
            1

        ModBits _ ->
            1

        ModFloat _ ->
            2

        ModDouble _ ->
            4


modValueFuzzer : Fuzzer ModValue
modValueFuzzer =
    Fuzz.oneOf
        [ Fuzz.map ModWord <| Fuzz.maybe Fuzz.int
        , Fuzz.map ModBits <| bitsFuzz
        , Fuzz.map ModFloat <| Fuzz.maybe <| Fuzz.map fromFloat Fuzz.float
        ]



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



--------------------------------------------------------------------------------------------------
-- Bits
--------------------------------------------------------------------------------------------------


type alias Bits =
    { value : String
    }


bitsFromString : String -> Maybe Bits
bitsFromString str =
    let
        filtered =
            String.filter
                (\c -> c == '0' || c == '1')
                str
    in
    if String.isEmpty filtered then
        Nothing

    else
        Just <|
            Bits <|
                filtered


decodeBits : D.Decoder Bits
decodeBits =
    D.andThen
        (\str ->
            case bitsFromString str of
                Nothing ->
                    D.fail "No valid Bits"

                Just bits ->
                    D.succeed bits
        )
        D.string


bitsValidString : String -> Bool
bitsValidString str =
    let
        len =
            String.length str
    in
    len > 0 && len <= 16


bitsFuzz : Fuzzer (Maybe Bits)
bitsFuzz =
    let
        bit =
            Fuzz.oneOf [ Fuzz.constant '0', Fuzz.constant '1' ]

        list =
            Fuzz.map (List.repeat 16) bit

        str =
            Fuzz.map String.fromList list
    in
    Fuzz.map bitsFromString str


