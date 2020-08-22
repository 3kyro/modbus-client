module View exposing (view)

import Html exposing (Html, div, text, label)
import Html.Attributes exposing (..)

import Types exposing (..)

view : Model -> Html Msg
view model =
    div [ class "modData"]
        [ label [ class "modName" ]
            [ text model.name ]
        , label [ class "modRegType" ]
            [ text <| showRegType model.regType ]
        , label [ class "modAddress" ]
            [ text <| String.fromInt model.address ]
        , viewModType model.value
        , viewModValue model.value
        , label [ class "modDescription" ]
            [ text model.description ]
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