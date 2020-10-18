module Settings exposing
    ( Setting
    , SettingInput(..)
    , SettingInputUpdateValue(..)
    , SettingStatus(..)
    , renderSettings
    , updateIndexedSetting
    )

import Array
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , alignTop
        , centerX
        , centerY
        , column
        , fill
        , focused
        , height
        , mouseOver
        , none
        , padding
        , paddingXY
        , px
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region exposing (description)
import Palette
    exposing
        ( darkGrey
        , grey
        , greyWhite
        , lightGrey
        )


type alias Setting value msg =
    { description : String
    , status : SettingStatus
    , inputs : List (SettingInput value msg)
    }


type alias StatusMessage value msg =
    Setting value msg -> msg


type alias CheckboxMessage msg =
    Int -> Int -> Bool -> msg


type alias NumberInputMessage msg =
    Int -> Int -> String -> msg


type alias RadioOptionMessage value msg =
    Int -> Int -> value -> msg


type SettingStatus
    = Active
    | NotActive


type SettingInput value msg
    = CheckBox
        { description : String
        , flag : Bool
        , message : CheckboxMessage msg
        }
    | NumberInput
        { description : String
        , value : Maybe Int
        , message : NumberInputMessage msg
        }
    | Radio
        { description : String
        , values : List ( value, String )
        , selected : Maybe value
        , message : RadioOptionMessage value msg
        }
    | EmptyInput


renderSettings : StatusMessage value msg -> List (Setting value msg) -> Element msg
renderSettings message settings =
    column
        [ Background.color grey
        , width fill
        , spacing 10
        ]
    <|
        List.indexedMap (renderSetting message) settings


renderSetting : StatusMessage value msg -> Int -> Setting value msg -> Element msg
renderSetting message parentIdx setting =
    column
        [ Background.color <| settingBGcolor setting.status
        , width fill
        , paddingXY 20 20
        , spacing 20
        , Font.color greyWhite
        , mouseOver [ Background.color <| hoverBGColor setting.status ]
        , onClick <| message setting
        ]
    <|
        text setting.description
            :: List.indexedMap (renderSettingInput parentIdx) setting.inputs


settingBGcolor : SettingStatus -> Color
settingBGcolor settingStatus =
    case settingStatus of
        Active ->
            darkGrey

        NotActive ->
            grey


hoverBGColor : SettingStatus -> Color
hoverBGColor settingStatus =
    case settingStatus of
        Active ->
            darkGrey

        NotActive ->
            lightGrey


renderSettingInput : Int -> Int -> SettingInput value msg -> Element msg
renderSettingInput parentIdx idx input =
    case input of
        CheckBox cb ->
            Input.checkbox
                []
                { onChange = cb.message parentIdx idx
                , icon = Input.defaultCheckbox
                , checked = cb.flag
                , label = Input.labelRight [] (text cb.description)
                }

        NumberInput ni ->
            Input.text
                [ width <| px 500
                , height <| px 32
                , centerY
                , Background.color lightGrey
                , Font.color greyWhite
                , focused [ Border.glow greyWhite 1 ]
                , paddingXY 5 8
                ]
                { onChange = ni.message parentIdx idx
                , text = Maybe.withDefault "" <| Maybe.map String.fromInt ni.value
                , placeholder = Nothing
                , label = Input.labelLeft [ centerY ] (text ni.description)
                }

        Radio ro ->
            Input.radio
                [ padding 10
                , spacing 20
                ]
                { onChange = ro.message parentIdx idx
                , selected = ro.selected
                , label = Input.labelAbove [] (text ro.description)
                , options = List.map toRadioOption ro.values
                }

        _ ->
            none


toRadioOption : ( value, String ) -> Input.Option value msg
toRadioOption ( val, str ) =
    Input.option val (text str)



-- An SettingInput appropriate value abstraction. Used for updating a setting input


type SettingInputUpdateValue value
    = CheckBoxValue Bool
    | NumberInputValue (Maybe Int)
    | RadioValue (Maybe value)



-- Update a setting input, using a input appropriate value


updateSettingInput : SettingInput value msg -> SettingInputUpdateValue value -> SettingInput value msg
updateSettingInput settingInput updateValue =
    case settingInput of
        CheckBox cb ->
            case updateValue of
                CheckBoxValue newFlag ->
                    CheckBox { cb | flag = newFlag }

                _ ->
                    settingInput

        NumberInput ni ->
            case updateValue of
                NumberInputValue newValue ->
                    NumberInput { ni | value = newValue }

                _ ->
                    settingInput

        Radio ro ->
            case updateValue of
                RadioValue newValue ->
                    Radio { ro | selected = newValue }

                _ ->
                    settingInput

        _ ->
            settingInput



-- Updates a setting using the setting's index as well as the index of the setting's input that has been modified


updateIndexedSetting : List (Setting value msg) -> Int -> Int -> SettingInputUpdateValue value -> Maybe (List (Setting value msg))
updateIndexedSetting initSettings settingIdx inputIdx updateValue =
    let
        -- Convert the global Settings list in an array
        arrSettings =
            Array.fromList initSettings

        -- Get the wanted Setting from that array
        mSetting =
            Array.get settingIdx arrSettings

        -- Get the list of SettingInputs from that Setting
        mSettingInputs =
            Maybe.map (\set -> set.inputs) mSetting

        -- Convert the list of SettingInputs to an Array
        mArrSettingInputs =
            Maybe.map Array.fromList mSettingInputs

        -- Find the looked for SettingInput and modify it
        mSettingInput =
            mArrSettingInputs
                |> Maybe.andThen (Array.get inputIdx)

        newSettingInput =
            Maybe.map
                (\si -> updateSettingInput si updateValue)
                mSettingInput

        -- Use this modified value to create a new modified list of SettingInputs
        mModifiedSettingInputs =
            Maybe.andThen
                (\setInput ->
                    Maybe.map
                        (\listSetInput ->
                            Array.set inputIdx setInput listSetInput
                        )
                        mArrSettingInputs
                )
                newSettingInput
                |> Maybe.map Array.toList

        -- get the modified Setting
        mModifiedSetting =
            Maybe.andThen
                (\setInputs ->
                    Maybe.map
                        (\setting -> { setting | inputs = setInputs })
                        mSetting
                )
                mModifiedSettingInputs

        -- use the modified Setting to get a modified list of Settings
    in
    Maybe.map
        (\setting ->
            Array.set settingIdx setting arrSettings
        )
        mModifiedSetting
        |> Maybe.map Array.toList
