module Update exposing (update)

import Http

import Json.Decode as D
import Json.Encode as E

import Types exposing
    ( Msg (..)
    , Model
    , Register (..)
    , getRegType
    , showRegValueType
    , getRegValue
    , ModValue (..)
    , Status (..)
    )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        ReadRegisters (Ok regs) ->
            ( { model | registers = regs , status = AllGood } , Cmd.none )
        ReadRegisters (Err _) ->
            ( { model | status = Bad "Something bad happened" }, Cmd.none )
        RefreshRequest regs -> ( { model | status = Loading } , refreshRequest regs)


refreshRequest : List Register -> Cmd Msg
refreshRequest regs =
    Http.post
        { url = "http://localhost:4000/register"
        , body = Http.jsonBody <| E.list encodeRegister regs
        , expect = Http.expectJson ReadRegisters <| D.list registerDecoder
        }

encodeRegister : Register -> E.Value
encodeRegister reg =
    E.object
        [ ( "regType" , E.string (getRegType reg))

        , ( "valueType" , E.string (showRegValueType reg))
        , ( "value" , encodeRegValue reg)
        ]

encodeRegValue : Register -> E.Value
encodeRegValue reg =
    case getRegValue reg of
        ModWord (Just v) -> E.int v
        ModFloat (Just v) -> E.float v
        _ -> E.null

decodeModValue : D.Decoder ModValue
decodeModValue =
    D.string |> D.andThen
        ( \s ->
            case s of
                "word" -> D.map ModWord <| D.nullable D.int
                "float" -> D.map ModFloat <| D.nullable D.float
                _ -> D.fail "not a valid ModValue"
        )
    -- decode a string and then decode maybe int or maybe float

registerDecoder : D.Decoder Register
registerDecoder =
    D.string |> D.andThen  (\s ->
        case s of
            "Input Register" -> D.map InputRegister decodeModValue
            "Holding Register" -> D.map HoldingRegister decodeModValue
            _ -> D.fail "Not a valid register"
    )




