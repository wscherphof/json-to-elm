module Main exposing (..)

import Home exposing (defaultModel, update, view, subscriptions)
import Html


main : Program Never Home.Model Home.Action
main =
    Html.program
        { init = ( defaultModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
