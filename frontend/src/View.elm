module View exposing (view)

import Html exposing (Html, div, text, label)
import Html.Attributes exposing (..)

import Types exposing (..)

view : Model -> Html Msg
view model =
    div [ class "modData"] (List.map viewModData model.modData)


viewModData : ModData -> Html Msg
viewModData data =
    div [ class "modData"]
        [ label [ class "modName" ]
            [ text data.name ]
        , label [ class "modRegType" ]
            [ text <| showRegister data.register ]
        , label [ class "modAddress" ]
            [ text <| String.fromInt data.address ]
        , viewModType <| getValue data.register
        , viewModValue <| getValue data.register
        , label [ class "modDescription" ]
            [ text data.description ]
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