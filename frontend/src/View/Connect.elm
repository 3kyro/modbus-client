module View.Connect exposing (viewConnectMenu)

import Html exposing
    ( Html
    , div
    , text
    , input
    , label
    , table
    , tbody
    , tr
    , td
    )
import Html.Attributes exposing
    ( class
    , value
    , size
    , maxlength
    )
import Html.Events exposing (onClick, onInput)

import Types exposing
    ( Model
    , Msg (..)
    , ConnectStatus(..)
    , showConnectStatus
    )
import Types.IpAddress exposing
    ( IpAddress
    , IpAddressByte (..)
    , showIpAddressByte
    )


viewConnectMenu : Model -> Html Msg
viewConnectMenu model =
    div [ class "activeMenu" , class "connectMenu" ]
        [ table [ class "connectInput" ]
            [ tbody []
                [ tr [ class "ipAddress" ]
                    [ td [] [ label [ size 5 ] [ text "IP address" ] ]
                    , td []
                        [
                        viewByteInput Byte0 model.ipAddress
                        , label [] [text "."]
                        , viewByteInput Byte1 model.ipAddress
                        , label [] [text "."]
                        , viewByteInput Byte2 model.ipAddress
                        , label [] [text "."]
                        , viewByteInput Byte3 model.ipAddress
                        ]
                    ]
                , tr [ class "port" ]
                    [ td [] [label [ size 50 ] [ text "Port" ]]
                    , td []
                        [ input
                            [ class "connectValueInput"
                            , size 2
                            , maxlength 4
                            , value <| Maybe.withDefault "" <| Maybe.map String.fromInt model.socketPort
                            , onInput <| ChangePort
                            ] []
                        ]
                    ]
                , tr [ class "timeout" ]
                    [ td [] [ label [ size 5 ] [ text "Timeout" ] ]
                    , td []
                        [ input
                            [ class "connectValueInput"
                            , size 2
                            , maxlength 5
                            , value <| Maybe.withDefault "" <| Maybe.map String.fromInt model.timeout
                            , onInput <| ChangeTimeout
                            ] []
                        ]
                    ]
                ]
            ]
        -- end div
        , div [ class "connectButtons" ]
            [ viewConnectButton model
            , viewDisconnectButton model
            ]
        ]

getDisconnectClass : ConnectStatus -> String
getDisconnectClass status =
    case status of
        Connected -> "Connect"
        _ -> "Disconnect"

viewByteInput : IpAddressByte -> IpAddress -> Html Msg
viewByteInput byte ip =
    input
        [ class "connectValueInput"
        ,  Html.Attributes.max "255"
        , Html.Attributes.min "0"
        , Html.Attributes.maxlength 3

        , Html.Attributes.size 1
        , value <| showIpAddressByte byte ip
        , onInput <| ChangeIpAddress byte
        ] []

viewDisconnectButton : Model -> Html Msg
viewDisconnectButton model =
    div
        [ class <| getDisconnectClass model.connectStatus
        , onClick <| DisconnectRequest
        ]
        [ text "Disconnect" ]

viewConnectButton : Model -> Html Msg
viewConnectButton model =
    div
        [ class <| showConnectStatus model.connectStatus
        , onClick <| ConnectRequest
        ]
        [ text <| showConnectStatus model.connectStatus ]


