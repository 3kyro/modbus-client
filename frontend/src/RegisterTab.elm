module RegisterTab exposing (renderRegistersTab)

import Dropdown exposing (dropdown)
import Element
    exposing
        ( Element
        , alignTop
        , paddingXY
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Input as Input
import Palette exposing (blueSapphire, fireBrick)
import ReadWrite
    exposing
        ( ReadWrite(..)
        , flipRW
        , readWriteButton
        )
import Types
    exposing
        ( Model
        , Msg(..)
        , getModValueUpdate
        )


renderRegistersTab : Model -> Element Msg
renderRegistersTab model =
    row
        [ spacing 20
        , paddingXY 20 20
        , alignTop
        ]
    <|
        [ text "Register type: "
        , dropdown
            []
            model.regTypeDd
        , text "Address"
        , Input.text
            [ width <| px 100 ]
            { onChange = RegAddress
            , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.regAddress
            , placeholder = Nothing
            , label = Input.labelHidden "Register Address"
            }
        , text "Value Type"
        , dropdown
            []
            model.valueTypeDd
        , text "Unit id"
        , Input.text
            [ width <| px 100 ]
            { onChange = RegUid
            , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.regUid
            , placeholder = Nothing
            , label = Input.labelHidden "Unit Id"
            }
        , readWriteButton
            model.regRW
            blueSapphire
            fireBrick
          <|
            RegToggleRW <|
                flipRW model.regRW
        ]
            ++ regNumInput model


regNumInput : Model -> List (Element Msg)
regNumInput model =
    case model.regRW of
        Read ->
            [ text "Number of registers"
            , Input.text
                [ width <| px 100 ]
                { onChange = RegNumber
                , text = Maybe.withDefault "" <| Maybe.map String.fromInt model.regNumReg
                , placeholder = Nothing
                , label = Input.labelHidden "Number"
                }
            ]

        Write ->
            [ text "Value"
            , Input.text
                [ width <| px 100 ]
                { onChange = RegModValue
                , text = Maybe.withDefault "" <| getModValueUpdate model.regMdu
                , placeholder = Nothing
                , label = Input.labelHidden "Value"
                }
            ]
