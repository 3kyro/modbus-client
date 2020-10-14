module Settings exposing
    ( Setting
    , SettingStatus (..)
    , SettingInput (..)
    , renderSettings
    , dummySetting
    )

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
