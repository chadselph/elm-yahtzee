module Dice exposing (Dice, Die, asNumberList, rollAll, rollUnheld, view)

import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onClick)
import Random
import String exposing (fromChar)


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
    Random.map (\_ -> x) (Random.int 0 1)


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
    span [ style "float" "right" ]
        [ label []
            [ input [ type_ "checkbox", onClick (toMsg updated), checked held ] []
            , text
                (if held then
                    "Holding"

                 else
                    "Rolling"
                )
            , drawDie ( i, held )
            ]
        ]


drawDie : Die -> Html s
drawDie ( i, held ) =
    div
        [ style "height" "75px"
        , style "width" "75px"
        , style "font-size" "50pt"
        , style "text-align" "center"
        , style "user-select" "none"
        , style "background-color" "rgb(211, 215, 207)"
        ]
        [ text (fromChar <| Char.fromCode (0x267F + i)) ]
