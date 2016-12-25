module Dice exposing (asNumberList, rollAll, rollUnheld, Dice, Die, view)

import Random
import String
import Text
import Element exposing (..)
import Color exposing (grey)
import Char
import Html exposing (Html, button, div, text, span, input, label)
import Html.Attributes exposing (type_, checked, class, style)
import Html.Events exposing (onClick)


type alias Held =
    Bool


type alias Die =
    ( Int, Held )


type Dice
    = Dice Die Die Die Die Die


type Action
    = FlipHeld Int Bool


asNumberList (Dice ( a, _ ) ( b, _ ) ( c, _ ) ( d, _ ) ( e, _ )) =
    [ a, b, c, d, e ]


asDieList (Dice a b c d e) =
    [ a, b, c, d, e ]


rollAll : Random.Seed -> ( Dice, Random.Seed )
rollAll =
    Random.step diceGenerator


nonRandom : a -> Random.Generator a
nonRandom x =
    Random.map (\_ -> x) Random.bool


maybeRoll : Die -> Random.Generator Die
maybeRoll (( _, hold ) as die) =
    if hold then
        nonRandom die
    else
        randomDie


rollUnheld : Dice -> Random.Seed -> ( Dice, Random.Seed )
rollUnheld dice seed =
    Random.step (unheldRollGenerator dice) seed


unheldRollGenerator : Dice -> Random.Generator Dice
unheldRollGenerator (Dice d1 d2 d3 d4 d5) =
    Random.map5 Dice
        (maybeRoll d1)
        (maybeRoll d2)
        (maybeRoll d3)
        (maybeRoll d4)
        (maybeRoll d5)


randomDie : Random.Generator Die
randomDie =
    let
        unheldDie n =
            ( n, False )
    in
        Random.map unheldDie (Random.int 1 6)


diceGenerator : Random.Generator Dice
diceGenerator =
    Random.map5 Dice randomDie randomDie randomDie randomDie randomDie


flipHeld ( i, held ) =
    ( i, not held )


view toMsg (Dice d1 d2 d3 d4 d5) =
    span [ class "dice" ]
        [ drawDieBox toMsg (Dice (flipHeld d1) d2 d3 d4 d5) d1
        , drawDieBox toMsg (Dice d1 (flipHeld d2) d3 d4 d5) d2
        , drawDieBox toMsg (Dice d1 d2 (flipHeld d3) d4 d5) d3
        , drawDieBox toMsg (Dice d1 d2 d3 (flipHeld d4) d5) d4
        , drawDieBox toMsg (Dice d1 d2 d3 d4 (flipHeld d5)) d5
        ]


drawDieBox toMsg updated ( i, held ) =
    span [ style [ ( "float", "right" ) ] ]
        [ label []
            [ input [ type_ "checkbox", onClick (toMsg updated), checked held ] []
            , text
                (if held then
                    "Holding"
                 else
                    "Rolling"
                )
            , drawDie ( i, held ) |> toHtml
            ]
        ]


drawDie : Die -> Element
drawDie ( i, held ) =
    color grey <|
        container 70 70 middle <|
            centered <|
                Text.height 70 <|
                    Text.fromString <|
                        String.fromChar <|
                            Char.fromCode (0x267F + i)
