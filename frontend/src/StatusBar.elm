module StatusBar exposing
    ( expandButton
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
import Types
    exposing
        ( Msg(..)
        , Notification
        , StatusBarState(..)
        )


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
    String.fromInt
        (Time.toHour zone posix)
        ++ ":"
        ++ (String.fromInt <| Time.toMinute zone posix)
        ++ ":"
        ++ (String.fromInt <| Time.toSecond zone posix)


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
