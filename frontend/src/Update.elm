module Update exposing (update)

import Types exposing (Msg, Model)

update : Msg -> Model -> ( Model, Cmd Msg)
update _ model = (model, Cmd.none)
