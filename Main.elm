import Graphics.Input as Input
import Graphics.Element (..)
import List
import Text (plainText)
import Text
import Signal
import Random
import Color (grey)
import String
import Char
import Time

import Dice (..)
import Scoresheet (..)

type Roll = FirstRoll | SecondRoll | LastRoll

type GameState = GameState Scoresheet Dice Roll Random.Seed


initialState seed =
  let (die, seed') = rollDice seed
  in GameState newScoresheet die FirstRoll seed'

states : Signal.Channel GameState
states = Signal.channel (initialState (Random.initialSeed 2))

newGame time = Signal.send states <|
  initialState <| Random.initialSeed <| round time

newGameButton time = Input.button (newGame time) "New Game"

signalChoice (GameState scoresheet dice roll seed) choice =
  let sheet' = choice dice scoresheet
      (dice', seed') = rollDice seed
  in Signal.send states <| GameState sheet' dice' FirstRoll seed'

textCol s = container 100 40 middle <| plainText <| s

scoresheet (GameState sb dice roll seed)=
  let boxColumn title s =
    let score box = case box of
          Played s -> textCol <| toString s
          Available setter ->
            Input.button (signalChoice (GameState sb dice roll seed) setter) "  -  "
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
       plainText "Lower Section",
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
        Text.centered <| Text.height 40 <| Text.fromString <|
        String.fromChar <| Char.fromCode (0x267f + i)

drawDice (GameState _ dice _ _) =
  diceToIntList dice |> List.map drawDie |> flow right

scene (time, gs) =
  flow right [scoresheet gs, spacer 100 100,
              flow down [
                drawDice gs,
                newGameButton time
              ]]

main = Signal.map scene (Time.timestamp (Signal.subscribe states))
