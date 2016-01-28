module Game (..) where

import Graphics.Input as Input
import Task
import Graphics.Element exposing (..)
import List
import Text exposing (defaultStyle)
import Text
import Signal
import Random
import Color exposing (grey)
import String
import Char
import Time
import Html exposing (fromElement)
import Time exposing (Time)
import Effects
import Effects exposing (Never)

import Dice exposing (..)
import Scoresheet exposing (..)
import StartApp

type Roll = FirstRoll | SecondRoll | LastRoll


type GameState = GameState Scoresheet Dice Roll Random.Seed

type Action = UpdateScorecard (Dice -> Scoresheet -> Scoresheet) | NewGame | Init Time


initialState = GameState newScoresheet (fst (rollDice (Random.initialSeed 0))) FirstRoll (Random.initialSeed 2)


newGameButton address = Input.button (Signal.message address NewGame) "New Game"

update action (GameState scoresheet dice roll seed) =
  let newstate = case action of
    UpdateScorecard (setter) ->
      let (dice', seed') = rollDice seed in
        GameState (setter dice scoresheet) dice' FirstRoll seed'
    NewGame ->
      let (dice', seed') = rollDice seed in
        GameState newScoresheet dice' FirstRoll seed'
    Init time ->
      let (dice', seed') = rollDice (Random.initialSeed (round time)) in
        GameState newScoresheet dice' FirstRoll seed'
  in (newstate, Effects.none)

textCol s = container 100 40 middle <| centered <| Text.fromString <| s

scoresheet (GameState sb dice roll seed) address =
  let boxColumn title s =
    let score box = case box of
          Played s -> textCol <| toString s
          Available setter ->
            Input.button (Signal.message address (UpdateScorecard setter)) " - "
    in
       flow right [textCol title, score s]
  in
     flow down [
       flow right <| List.map textCol ["Upper Section", "Score"],
       boxColumn "Aces" sb.aces,
       boxColumn "Twos" sb.twos,
       boxColumn "Threes" sb.threes,
       boxColumn "Four" sb.fours,
       boxColumn "Fives" sb.fives,
       boxColumn "Sixes" sb.sixes,
       flow right [textCol "Bonus", textCol <| toString <| upperBonus sb],
       centered <| Text.fromString "Lower Section",
       boxColumn "3 of a kind" sb.threeOfAKind,
       boxColumn "4 of a kind" sb.fourOfAKind,
       boxColumn "Full House" sb.fullHouse,
       boxColumn "Small Straight" sb.smallStraight,
       boxColumn "Large Straight" sb.largeStraight,

       boxColumn "Yahtzee" sb.yahtzee,
       boxColumn "Chance" sb.chance,
       flow right [textCol "Total", textCol <|toString <| totalScore sb]
       ]

drawDie i = color grey <| container 40 40 middle <|
        centered <| Text.height 40 <| Text.fromString <|
        String.fromChar <| Char.fromCode (0x267f + i)

drawDice (GameState _ dice _ _) =
  diceToIntList dice |> List.map drawDie |> flow right

view address gamestate =
  flow right [(scoresheet gamestate address), spacer 100 100,
              flow down [
                drawDice gamestate,
                newGameButton address
              ]]
  |> fromElement


view2 address model = drawDie model |> fromElement
update2 action model = (model, Effects.none)
