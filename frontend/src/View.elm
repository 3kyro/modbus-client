module View exposing (view)

import Html exposing
    ( Html
    , div
    , text
    , label
    , table
    , th
    , tr
    , td
    , thead
    , tbody
    , tfoot
    , button
    , input
    )
import Html.Attributes exposing
    ( class
    , scope
    , colspan
    , value
    , type_
    )
import Html.Events exposing (onClick, onInput)

import Types exposing
    ( Msg (..)
    , Model
    , RegType (..)
    , getRegType
    , ModValue (..)
    , Status (..)
    , showStatus
    , ModData
    , IpAddressByte(..)
    , showIpAddressByte
    )
import String

view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "connect" ] (viewConnect model)
        , div [ class "inputRegisters" ] [viewMultModData model.modData]
        , div [ class "status" ] [text <| showStatus model.status]
        ]

viewConnect : Model -> List (Html Msg)
viewConnect model =
    [ div []
        [ text "ip address"
        , viewByteInput <| Byte1 model.ipAddress.b1
        , viewByteInput <| Byte1 model.ipAddress.b2
        , viewByteInput <| Byte1 model.ipAddress.b3
        , viewByteInput <| Byte1 model.ipAddress.b4
        ]

    , div []
        [ text "port"
        , input [ type_ "number", value <| String.fromInt model.socketPort ] []
        ]
    , button [onClick <| ConnectRequest (model.ipAddress , model.socketPort)] [ text "connect"]
    ]

viewByteInput : IpAddressByte -> Html Msg
viewByteInput byte =
    input
        [ type_ "number"
        , Html.Attributes.max "255"
        , Html.Attributes.min "0"
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
                else ChangeIpAddressByte <| insertByte byte value

insertByte : IpAddressByte -> Int -> IpAddressByte
insertByte b i =
    case b of
        Byte1 _ -> Byte1 i
        Byte2 _ -> Byte2 i
        Byte3 _ -> Byte3 i
        Byte4 _ -> Byte4 i
        NoByte -> NoByte

viewMultModData : List ModData -> Html Msg
viewMultModData mds =
    div [ class "registers" ]
        [ table [ class "regTable" ]
            [ thead [] <|
                [ tr []
                    [ th [] [ text "Name"]
                    , th [] [ text "Type" ]
                    , th [] [ text "Address" ]
                    , th [] [ text "Value Type" ]
                    , th [] [ text "Value" ]
                    , th [] [ text "Unit Id" ]
                    , th [] [ text "Description" ]
                    ]
                ]
            , tbody [] (List.map viewModData mds)
            , tfoot [onClick <| RefreshRequest mds]
                [ tr []
                    [ th [scope "row", colspan 7 ] [ text "refresh"]
                    ]
                ]
            ]
        ]

viewModData : ModData -> Html Msg
viewModData md =
    tr []
        [ td [] [ text md.modName ]
        , td [] [ text <| getRegType md.modRegType ]
        , td [] [ text <| String.fromInt md.modAddress ]
        , td [] [ viewModType md.modValue ]
        , td [] [ viewModValue md.modValue ]
        , td [] [ text <| String.fromInt md.modUid ]
        , td [] [ text md.modDescription ]
        ]

viewModValue : ModValue -> Html Msg
viewModValue value =
    label [ class "modValue" ]
        <| case value of
            ModWord (Just word) ->
                    [ text <| String.fromInt word]
            ModFloat (Just float) ->
                    [ text <| String.fromFloat float]
            _ -> []
viewModType : ModValue -> Html Msg
viewModType value =
    label [ class "modType" ]
        <| case value of
            ModWord (_) ->
                    [ text "Word" ]
            ModFloat (_) ->
                    [ text "Float" ]