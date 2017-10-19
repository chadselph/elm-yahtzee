module Main exposing (..)

import Game
import Scoresheet exposing (Scoresheet)
import Task
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Platform.Sub


type State
    = InProgress Game.GameState
    | NewGameScreen
    | GameOver Int Scoresheet


type Msg
    = StartGame Int
    | GameEvent Game.Action
    | GetSeed


view state =
    case state of
        NewGameScreen ->
            button [ onClick GetSeed ] [ text "Begin" ]

        GameOver score scoresheet ->
            button [ onClick GetSeed ] [ text "Begin" ]

        InProgress gs ->
            Html.map GameEvent (Game.view gs)


update msg st =
    case msg of
        StartGame value ->
            ( InProgress (Game.initialState value), Cmd.none )

        GetSeed ->
            ( st, Random.generate StartGame (Random.int Random.minInt Random.maxInt) )

        GameEvent action ->
            case st of
                InProgress gamestate ->
                    ( InProgress (Game.update action gamestate), Cmd.none )

                other ->
                    ( other, Cmd.none )


subs model =
    Sub.none


main =
    Html.program
        { init = ( NewGameScreen, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subs
        }
