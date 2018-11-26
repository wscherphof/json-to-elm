module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Home exposing (defaultModel, update, view, subscriptions)


main : Program () Home.Model Home.Action
main =
    Browser.element
        { view = view
        , init = \_ -> ( defaultModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
