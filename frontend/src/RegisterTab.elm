module RegisterTab exposing
    ( regNav
    , renderOutput
    , sendRegRequestButton
    )

import Dropdown exposing (Dropdown, dropdown)
import Element
    exposing
        ( Color
        , Element
        , IndexedColumn
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , focused
        , height
        , indexedTable
        , padding
        , paddingXY
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ModData
    exposing
        ( ModDataUpdate
        , getModValueUpdate
        , modAddressColumn
        , modRegTypeColumn
        , modUidColumn
        , modValueColumn
        , modValueTypeColumn
        )
import NavigationModule
    exposing
        ( navButton
        , navDd
        , navInput
        )
import Palette exposing (background, blueSapphire, fireBrick, greyWhite, lightGrey)
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
        )


regNav : Model -> Element Msg
regNav model =
    column
        [ Background.color background
        , width fill
        , height fill
        , spacing 10
        ]
        [ navUid model
        , navRegTypeDb model
        , navAddress model
        , navValueType model
        , navRW model
        , navNumInput model
        , sendRegRequestButton
        ]


navUid : Model -> Element Msg
navUid model =
    navInput "Unit id" RegUid <| Maybe.map String.fromInt model.regUid


navRegTypeDb : Model -> Element Msg
navRegTypeDb model =
    navDd "Reg. Type" model.regTypeDd


navAddress : Model -> Element Msg
navAddress model =
    navInput "Address" RegAddress <| Maybe.map String.fromInt model.regAddress


navValueType : Model -> Element Msg
navValueType model =
    navDd "Value Type" model.regModValueDd


navRW : Model -> Element Msg
navRW model =
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
            text "Action"
        , navRWButton model
        ]


navNumInput : Model -> Element Msg
navNumInput model =
    case model.regMdu.mduRW of
        Read ->
            navInput "Number" RegNumber <| Maybe.map String.fromInt model.regNumReg

        Write ->
            navInput "Value" RegModValue <| getModValueUpdate model.regMdu


sendRegRequestButton : Element Msg
sendRegRequestButton =
    navButton "Update" (Just UpdateRegMdu)


navRWButton : Model -> Element Msg
navRWButton model =
    Input.button
        [ Background.color <| rwButtonClr model.regMdu.mduRW
        , width <| fillPortion 3
        , height <| px 38
        , Font.center
        , Font.color greyWhite
        , paddingXY 0 10
        , focused []
        ]
        { onPress = Just <| RegToggleRW <| flipRW model.regMdu.mduRW
        , label = text <| rwButtonText model.regMdu.mduRW
        }


rwButtonClr : ReadWrite -> Color
rwButtonClr rw =
    case rw of
        Read ->
            blueSapphire

        Write ->
            fireBrick


rwButtonText : ReadWrite -> String
rwButtonText rw =
    case rw of
        Read ->
            "Read"

        Write ->
            "Write"


renderOutput : List ModDataUpdate -> Element Msg
renderOutput mdus =
    indexedTable
        []
        { data = mdus
        , columns = responseColumns mdus
        }


responseColumns : List ModDataUpdate -> List (IndexedColumn ModDataUpdate Msg)
responseColumns mdus =
    [ modRegTypeColumn
    , modAddressColumn
    , modValueTypeColumn
    , modValueColumn Nothing
    , modUidColumn
    ]
