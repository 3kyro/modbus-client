module StatusBar exposing (renderNotifications)

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
    )
import Html.Attributes exposing (id)
import Types exposing
    ( StatusBarState(..)
    , Notification
    , Msg (..)
    )
import Time


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

