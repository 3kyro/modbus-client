module RegisterTab exposing
    ( regNav
    , registersHelpModule
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
        , paragraph
        , px
        , row
        , scrollbars
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ModData
    exposing
        ( ModDataUpdate
        , ModValue
        , RegType(..)
        , getModValueUpdate
        , isWriteableReg
        , modAddressColumn
        , modEmptyColumn
        , modRegTypeColumn
        , modUidColumn
        , modValueColumn
        , modValueTypeColumn
        , showModValueType
        , showRegType
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
import String
import Types
    exposing
        ( Model
        , Msg(..)
        )



----------------------------------------------------------------------------------------------------------------------------------
-- Register Navigtion Module
-----------------------------------------------------------------------------------------------------------------------------------


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
        [ Background.color <| rwButtonClr model
        , width <| fillPortion 3
        , height <| px 38
        , Font.center
        , Font.color greyWhite
        , paddingXY 0 10
        , focused []
        , Border.width <| getBorderWidth model
        ]
        { onPress = Just <| RegToggleRW <| flipRW model.regMdu.mduRW
        , label = text <| rwButtonText model.regMdu.mduRW
        }



-- Display a border when button is clickable


getBorderWidth : Model -> Int
getBorderWidth model =
    if isWriteableReg model.regTypeDd.selected.value then
        1

    else
        0



-- Change color to signify when button is clicable


rwButtonClr : Model -> Color
rwButtonClr model =
    if isWriteableReg model.regTypeDd.selected.value then
        case model.regMdu.mduRW of
            Read ->
                blueSapphire

            Write ->
                fireBrick

    else
        lightGrey


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
    , modEmptyColumn
    ]



----------------------------------------------------------------------------------------------------------------------------------
-- Register Navigtion Module
-----------------------------------------------------------------------------------------------------------------------------------
-- didplay register tab help messages


registersHelpModule : Model -> Element Msg
registersHelpModule model =
    paragraph
        [ alignTop
        , Font.color lightGrey
        ]
    <|
        regHelpText model.regMdu


regHelpText : ModDataUpdate -> List (Element Msg)
regHelpText mdu =
    let
        -- very crude plural conversion. Works only for the set of ModValues
        plural str =
            if String.endsWith "s" str then
                str

            else
                str ++ "s"
    in
    case mdu.mduRW of
        Read ->
            [ text <|
                "Read multiple "
                    ++ showRegType mdu.mduModData.modRegType
                    ++ " "
                    ++ plural (showModValueType mdu.mduModData.modValue)
                    ++ "."
            ]

        Write ->
            [ text <|
                "Write a single "
                    ++ showRegType mdu.mduModData.modRegType
                    ++ " "
                    ++ showModValueType mdu.mduModData.modValue
                    ++ "."
            ]
