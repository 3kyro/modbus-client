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
    , type_
    , value
    , size
    )
import Html.Events exposing (onClick, onInput)

import Types exposing
    ( Model
    , Msg (..)
    , IpAddressByte(..)
    , ConnectStatus(..)
    , showIpAddressByte
    , insertIpAddressByte
    , showConnectStatus
    )

viewConnectMenu : Model -> Html Msg
viewConnectMenu model =
    div [ class "activeMenu" , class "connectRegion" ]
        [ div []
            [ text "ip address"
            , viewByteInput <| Byte1 model.ipAddress.b1
            , label [] [text "."]
            , viewByteInput <| Byte2 model.ipAddress.b2
            , label [] [text "."]
            , viewByteInput <| Byte3 model.ipAddress.b3
            , label [] [text "."]
            , viewByteInput <| Byte4 model.ipAddress.b4
            ]


        , div []
            [ text "port"
            , input
                [ type_ "number"
                , size 4
                , value <| String.fromInt model.socketPort
                , onInput <| ChangePort
                ] []
            , text "timeout"
            , input
                [ type_ "number"
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

viewByteInput : IpAddressByte -> Html Msg
viewByteInput byte =
    input
        [ type_ "number"
        , Html.Attributes.max "255"
        , Html.Attributes.min "0"
        , Html.Attributes.size 3
        , value <| showIpAddressByte byte
        , onInput <| changeIp byte
        ] []

changeIp : IpAddressByte -> String -> Msg
changeIp byte s =
    let
        mbyte = String.toInt s
    in
        case mbyte of
            Nothing -> ChangeIpAddressByte NoByte
            Just value ->
                if value < 0 || value > 255
                then ChangeIpAddressByte NoByte
                else ChangeIpAddressByte <| insertIpAddressByte byte value

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


