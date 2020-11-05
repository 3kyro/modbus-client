module HeartBeat exposing (heartBeatInfoModule, heartBeatNav)

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , IndexedColumn
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , indexedTable
        , px
        , spacing
        , text
        , width
        , paddingXY
        , alignLeft
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import NavigationModule
    exposing
        ( navButton
        , navInput
        )
import Palette exposing (background, grey, greyWhite, lightGrey)
import Types
    exposing
        ( HeartBeat
        , Model
        , Msg(..)
        )


heartBeatNav : Model -> Element Msg
heartBeatNav model =
    column
        [ Background.color background
        , width fill
        , height fill
        , spacing 10
        ]
        [ navUid model
        , navAddress model
        , navInterval model
        , startButton
        , stopButton
        ]


navUid : Model -> Element Msg
navUid model =
    navInput "Unit id" HeartUid <| Maybe.map String.fromInt model.heartUid


navAddress : Model -> Element Msg
navAddress model =
    navInput "Address" HeartAddress <| Maybe.map String.fromInt model.heartAddr


navInterval : Model -> Element Msg
navInterval model =
    navInput "Interval" HeartInterval <| Maybe.map String.fromInt model.heartIntv


startButton : Element Msg
startButton =
    navButton "Start" (Just StartHeartBeat)


stopButton : Element Msg
stopButton =
    navButton "Stop" (Just StopHeartBeat)


heartBeatInfoModule : Model -> Element Msg
heartBeatInfoModule model =
    indexedTable
        []
        { data = model.heartbeats
        , columns = heartbeatColumns model
        }


heartbeatColumns : Model -> List (IndexedColumn HeartBeat Msg)
heartbeatColumns model =
    [ selectColumn model
    , uidColumn
    , addressColumn
    , intervalColumn
    ]


uidColumn : IndexedColumn HeartBeat Msg
uidColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Unit Id"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.uid
    }


addressColumn : IndexedColumn HeartBeat Msg
addressColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Address"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.address
    }


intervalColumn : IndexedColumn HeartBeat Msg
intervalColumn =
    { header = el [ height <| px 38 ] <| el headerTextAttr <| text "Interval"
    , width = fillPortion 1
    , view = \i md -> viewCell i <| String.fromInt md.interval
    }


headerTextAttr : List (Attribute msg)
headerTextAttr =
    [ centerX, centerY ]


viewCell : Int -> String -> Element msg
viewCell idx str =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        ]
        (el [ centerX, centerY ] <| text str)


tableCellColor : Int -> Color
tableCellColor idx =
    if modBy 2 idx == 0 then
        lightGrey

    else
        grey

selectColumn : Model -> IndexedColumn HeartBeat Msg
selectColumn model =
    { header =
        el
            [ height <| px 38
            , paddingXY 10 0
            ]
        <|
            selectCheckbox SelectAllChecked model.heartSelectAll
    , width = px 30
    , view = \i hb -> viewCheckedCell i hb.selected
    }

selectCheckbox : (Bool -> Msg) -> Bool -> Element Msg
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

viewCheckedCell : Int -> Bool -> Element Msg
viewCheckedCell idx selected =
    el
        [ Background.color <| tableCellColor idx
        , Font.color greyWhite
        , height <| px 38
        , Font.center
        , paddingXY 10 0
        ]
    <|
        selectCheckbox (HeartBeatChecked idx) selected
