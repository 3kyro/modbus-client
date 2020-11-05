module NavigationModule exposing (navButton, navDd, navInput)

import Dropdown exposing (Dropdown, dropdown)
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , fillPortion
        , focused
        , height
        , padding
        , paddingXY
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Palette exposing (background, blueSapphire, fireBrick, greyWhite, lightGrey)


navInput : String -> (String -> msg) -> Maybe String -> Element msg
navInput label msg mvalue =
    row
        [ width fill
        , height <| px 38
        , spacing 10
        ]
        [ el
            [ width <| fillPortion 2
            , Background.color lightGrey
            , padding 11
            , height fill
            ]
          <|
            text label
        , el
            [ width <| fillPortion 3
            , Background.color lightGrey
            , height <| px 38
            ]
          <|
            Input.text
                [ Background.color lightGrey
                , width fill
                , height fill
                , Font.center
                , Font.color greyWhite
                ]
                { onChange = msg
                , text = Maybe.withDefault "" <| mvalue
                , placeholder = Nothing
                , label = Input.labelHidden label
                }
        ]


navDd : String -> Dropdown value msg -> Element msg
navDd label db =
    row
        [ width fill
        , height <| px 38
        , spacing 10
        ]
        [ el
            [ width <| fillPortion 2
            , Background.color lightGrey
            , padding 11
            , height fill
            ]
          <|
            text label
        , el
            [ width <| fillPortion 3
            , Background.color lightGrey
            , height <| px 38
            ]
          <|
            dropdown
                [ Background.color blueSapphire
                , width fill
                , padding 11
                , height <| px 38
                , Font.center
                , Font.color greyWhite
                ]
                db
        ]


navButton : String -> Maybe msg -> Element msg
navButton label msg =
    Input.button
        [ Background.color lightGrey
        , width fill
        , height <| px 38
        , Font.center
        , Font.color greyWhite
        , paddingXY 0 10
        , focused []
        ]
        { onPress = msg
        , label = text label
        }
