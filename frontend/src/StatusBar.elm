module StatusBar exposing
    ( renderNotifications
    , expandButton
    )

import Element exposing
    ( Element
    , row
    , el
    , text
    , none
    , column
    , Length
    , px
    , height
    , scrollbarY
    , centerY
    , width
    , alignBottom
    , clipX
    , fill
    , htmlAttribute
    , focused
    )
import Html.Attributes exposing (id)
import Types exposing
    ( StatusBarState(..)
    , Notification
    , Msg (..)
    )
import Time
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Palette exposing
    ( darkGrey
    , black
    )

renderNotification : Time.Zone -> Notification -> Element msg
renderNotification zone not =
    row
        [ height <| px 20
        , alignBottom
        ]
        [ renderTime not zone
        ]

renderTime : Notification -> Time.Zone -> Element msg
renderTime not zone =
    el
    [ centerY
    , width fill
    ]
    <| text <| hhmmss zone not.time

hhmmss : Time.Zone -> Time.Posix -> String
hhmmss zone posix =
    String.fromInt
        (Time.toHour zone posix) 
        ++ ":"
        ++ (String.fromInt <| Time.toMinute zone posix)
        ++ ":"
        ++ (String.fromInt <| Time.toSecond zone posix)

renderNotifications : Time.Zone -> StatusBarState -> List Notification -> Element msg
renderNotifications zone state notifications =
    let
        elements =
            case state of
                Expanded -> List.reverse <| List.map ( renderNotification zone ) notifications
                Retracted ->
                    List.singleton <|
                    Maybe.withDefault
                        none
                        <| Maybe.map
                            ( renderNotification zone )
                            <| List.head notifications
    in
        column
            [ height <| statusBarHeight state
            , scrollbarY
            , clipX
            , htmlAttribute <| id "status"
            ]
            elements

statusBarHeight : StatusBarState -> Length
statusBarHeight state =
    case state of
        Expanded -> px 100
        Retracted -> px 20


expandButton : StatusBarState -> Element Msg
expandButton state =
    Input.button
        [ Background.color darkGrey
        , width fill
        , height <| px 20
        , Font.center
        , focused [ Border.glow black 0 ]
        ]
        { onPress = Just <| ExpandStatus
        , label = expandButtonLabel state
        }


expandButtonLabel : StatusBarState -> Element Msg
expandButtonLabel state =
    case state of
        Expanded ->
            -- "▼" \u{25BC}'
            text "▼"

        Retracted ->
            -- "▲" '\u{25B2}'
            text <| String.fromChar '▲'