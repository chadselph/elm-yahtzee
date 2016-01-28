module Dice
    (diceToIntList, rollDice, Dice, Die)
    where

import Random

type alias Die = Int
type Dice = Dice Die Die Die Die Die

diceToIntList (Dice a b c d e) = [a,b,c,d,e]

rollDice : Random.Seed -> (Dice, Random.Seed)
rollDice seed = Random.generate diceGenerator seed

randomDie = Random.int 1 6

diceGenerator : Random.Generator Dice
diceGenerator = Random.map5 Dice randomDie randomDie randomDie randomDie randomDie
