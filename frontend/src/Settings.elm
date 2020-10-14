module Settings exposing
    ( Setting
    , SettingStatus (..)
    , SettingInput (..)
    , renderSettings
    , dummySetting
    , updateCheckboxSetting
    )

import Array
import Element
    exposing
        ( Element
        , Attribute
        , centerX
        , column
        , fill
        , height
        , mouseOver
        , none
        , px
        , text
        , width
        , Color
        , paddingXY
        , spacing
        , alignTop
        )

import Element.Events exposing (onClick)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Palette
    exposing
        ( darkGrey
        , grey
        , lightGrey
        , greyWhite
        )


type alias Setting msg =
    { description   : String
    , status        : SettingStatus
    , inputs        : List (SettingInput msg)
    }

type alias StatusMessage msg = Setting msg -> msg
type alias CheckboxMessage msg = Int -> Int -> Bool -> msg

type SettingStatus
    = Active
    | NotActive

type SettingInput msg
    = CheckBox
        { description : String
        , flag : Bool
        , message : CheckboxMessage msg
        }
    | DropDown
    | EmptyInput

dummySetting : Setting msg
dummySetting =
    Setting "This is a dummy setting" NotActive [EmptyInput]


renderSettings : StatusMessage msg -> List (Setting msg) ->  Element msg
renderSettings message settings =
    column
        [ Background.color grey
        , width fill
        , spacing 10
        ]
    <|
        List.indexedMap (renderSetting message) settings


renderSetting : StatusMessage msg -> Int -> Setting msg ->  Element msg
renderSetting message parentIdx setting =
    column
        [ Background.color <| settingBGcolor setting.status
        , width fill
        , paddingXY 20 20
        , Font.color greyWhite
        , mouseOver [ Background.color <| hoverBGColor setting.status ]
        , onClick <| message setting
        ]
        <| (text setting.description) :: List.indexedMap (renderCheckbox parentIdx) setting.inputs

settingBGcolor : SettingStatus -> Color
settingBGcolor settingStatus =
    case settingStatus of
        Active -> darkGrey
        NotActive -> grey

hoverBGColor : SettingStatus -> Color
hoverBGColor settingStatus =
    case settingStatus of
        Active -> darkGrey
        NotActive -> lightGrey

renderCheckbox : Int -> Int ->  SettingInput msg -> Element msg
renderCheckbox parentIdx idx input =
    case input of
        CheckBox cb ->
            Input.checkbox
                []
                { onChange = cb.message parentIdx idx
                , icon = Input.defaultCheckbox
                , checked = cb.flag
                , label = Input.labelRight [] (text cb.description)
                }
        _ -> none

updateCheckboxSetting : List (Setting msg) -> Int -> Int -> Bool -> Maybe (List (Setting msg))
updateCheckboxSetting initSettings settingIdx inputIdx newFlag =
    let
        -- Convert the global Settings list in an array
        arrSettings = Array.fromList initSettings
        -- Get the wanted Setting from that array
        mSetting = Array.get settingIdx arrSettings
        -- Get the list of SettingInputs from that Setting
        mSettingInputs =
            Maybe.map (\set -> set.inputs) mSetting
        -- Convert the list of SettingInputs to an Array
        mArrSettingInputs = Maybe.map Array.fromList mSettingInputs
        -- Find the looked for SettingInput and modify it
        mSettingInput =
            mArrSettingInputs
            |> Maybe.andThen (Array.get inputIdx)
        newSettingInput =
            Maybe.map
                (\si ->
                    case si of
                        CheckBox cb -> CheckBox { cb | flag = newFlag }
                        _ -> si
                )
                mSettingInput

        -- Use this modified value to create a new modified list of SettingInputs
        mModifiedSettingInputs =
            Maybe.andThen
                (\setInput -> Maybe.map
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
                (\setInputs -> Maybe.map
                    (\setting -> { setting | inputs = setInputs } )
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
