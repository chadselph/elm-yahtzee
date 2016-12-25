module Main exposing (..)

import Game
import Task
import Html exposing (Html, button, div, text)


main =
    Html.beginnerProgram
        { model = Game.initialState
        , update = Game.update
        , view = Game.view
        }
