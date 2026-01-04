module Main exposing (..)

import Browser
import Game
import Html exposing (button, text)
import Html.Events exposing (onClick)
import Random
import Scoresheet exposing (Scoresheet)


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


subs : State -> Sub Msg
subs _ =
    Sub.none


init : () -> ( State, Cmd Msg )
init _ =
    ( NewGameScreen, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }
