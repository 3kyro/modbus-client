module Notifications exposing
    ( Notification
    , StatusBarState (..)
    , expandButton
    , renderNotifications

    )

import Element
    exposing
        ( Element
        , Length
        , alignBottom
        , centerY
        , clipX
        , column
        , el
        , fill
        , focused
        , height
        , htmlAttribute
        , none
        , px
        , row
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes exposing (id)
import Palette
    exposing
        ( black
        , blueSapphire
        , darkGrey
        )
import Time

type StatusBarState
    = Expanded
    | Retracted

type alias Notification =
    { time : Time.Posix
    , header : String
    , detailed : Maybe String
    , expanded : Bool
    }


renderNotification : Time.Zone -> Notification -> Element msg
renderNotification zone not =
    row
        [ height <| px 20
        , alignBottom
        , width fill
        , spacing 20
        ]
        [ renderTime not zone
        , renderHeader not
        ]


renderTime : Notification -> Time.Zone -> Element msg
renderTime not zone =
    el
        [ centerY
        ]
    <|
        text <|
            hhmmss zone not.time


hhmmss : Time.Zone -> Time.Posix -> String
hhmmss zone posix =
    (formatTime <| Time.toHour zone posix)
        ++ ":"
        ++ (formatTime <| Time.toMinute zone posix)
        ++ ":"
        ++ (formatTime <| Time.toSecond zone posix)



-- Format a time unit, making sure it
-- is always displayed with two digits
formatTime : Int -> String
formatTime unit =
    String.padLeft 2 '0' <|
        String.fromInt unit


renderHeader : Notification -> Element msg
renderHeader not =
    el
        [ centerY
        ]
    <|
        text <|
            not.header


renderNotifications : Time.Zone -> StatusBarState -> List Notification -> Element msg
renderNotifications zone state notifications =
    let
        elements =
            case state of
                Expanded ->
                    List.reverse <| List.map (renderNotification zone) notifications

                Retracted ->
                    List.singleton <|
                        Maybe.withDefault
                            none
                        <|
                            Maybe.map
                                (renderNotification zone)
                            <|
                                List.head notifications
    in
    column
        [ Background.color blueSapphire
        , height <| statusBarHeight state
        , scrollbarY
        , clipX
        , width fill
        , htmlAttribute <| id "status"
        ]
        elements


statusBarHeight : StatusBarState -> Length
statusBarHeight state =
    case state of
        Expanded ->
            px 100

        Retracted ->
            px 20


expandButton : StatusBarState -> msg -> Element msg
expandButton state cmd =
    Input.button
        [ Background.color darkGrey
        , width fill
        , height <| px 20
        , Font.center
        , focused [ Border.glow black 0 ]
        ]
        { onPress = Just <| cmd
        , label = expandButtonLabel state
        }


expandButtonLabel : StatusBarState -> Element msg
expandButtonLabel state =
    case state of
        Expanded ->
            -- "▼" '\u{25BC}'
            text <| String.fromChar '\u{25BC}'

        Retracted ->
            -- "▲" '\u{25B2}'
            text <| String.fromChar '\u{25B2}'
