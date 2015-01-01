module Dice
    (diceToIntList, diceFromIntList, rollDice, Dice, Die)
    where

import Random

type alias Die = Int
type Dice = Dice Die Die Die Die Die

diceToIntList (Dice a b c d e) = [a,b,c,d,e]
diceFromIntList [a,b,c,d,e] = Dice a b c d e

rollDice : Random.Seed -> (Dice, Random.Seed)
rollDice seed = let generator = Random.list 5 (Random.int 1 6)
                in let (nums, seed') = Random.generate generator seed
                   in (diceFromIntList nums, seed')

