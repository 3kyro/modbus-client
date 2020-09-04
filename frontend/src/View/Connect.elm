module View.Connect exposing (viewConnectMenu)

import Html exposing
    ( Html
    , div
    , text
    , input
    , label
    )
import Html.Attributes exposing
    ( class
    , value
    , size
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
    , changeIpAddressByte
    )

viewConnectMenu : Model -> Html Msg
viewConnectMenu model =
    div [ class "activeMenu" , class "connectRegion" ]
        [ div []
            [ text "ip address"
            , viewByteInput Byte1 model.ipAddress
            , label [] [text "."]
            , viewByteInput Byte2 model.ipAddress
            , label [] [text "."]
            , viewByteInput Byte3 model.ipAddress
            , label [] [text "."]
            , viewByteInput Byte4 model.ipAddress
            ]
        , div
            []
            [ text "port"
            , input
                [ class "connectInput"
                , size 4
                , value <| String.fromInt model.socketPort
                , onInput <| ChangePort
                ] []
            , text "timeout"
            , input
                [ class "connectInput"
                , size 5
                , value <| String.fromInt model.timeout
                , onInput <| ChangeTimeout
                ] []
            ]
        , viewConnectButton model
        , viewDisconnectButton model
        ]

getDisconnectClass : ConnectStatus -> String
getDisconnectClass status =
    case status of
        Connected -> "connect"
        _ -> "disconnect"

viewByteInput : IpAddressByte -> IpAddress -> Html Msg
viewByteInput byte ip =
    input
        [ class "connectInput"
        ,  Html.Attributes.max "255"
        , Html.Attributes.min "0"
        , Html.Attributes.size 3
        , value <| showIpAddressByte byte ip
        , onInput <| changeIpAddress byte ip
        ] []

changeIpAddress : IpAddressByte -> IpAddress -> String -> Msg
changeIpAddress byte ip s =
    ChangeIpAddress <| changeIpAddressByte byte ip s


viewDisconnectButton : Model -> Html Msg
viewDisconnectButton model =
    div
        [ class <| getDisconnectClass model.connectStatus
        , onClick <| DisconnectRequest
        ]
        [ text "disconnect" ]

viewConnectButton : Model -> Html Msg
viewConnectButton model =
    div
        [ class <| showConnectStatus model.connectStatus
        , onClick <| ConnectRequest
        ]
        [ text <| showConnectStatus model.connectStatus ]


