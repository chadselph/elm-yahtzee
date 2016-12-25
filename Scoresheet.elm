module Scoresheet exposing (newScoresheet, totalScore, upperBonus, Scoresheet, Box(..), countSequential)

import List exposing (..)
import Dice
import Dice exposing (Dice)
import Maybe
import Dict


type alias Score =
    Int


type Box
    = Available (Dice -> Scoresheet -> Scoresheet)
    | Played Score


type alias Scoresheet =
    { -- upper
      aces : Box
    , twos : Box
    , threes : Box
    , fours : Box
    , fives : Box
    , sixes : Box
    , -- lower
      threeOfAKind : Box
    , fourOfAKind : Box
    , fullHouse : Box
    , smallStraight : Box
    , largeStraight : Box
    , yahtzee : Box
    , chance : Box
    }


newScoresheet : Scoresheet
newScoresheet =
    let
        boxUpdate setField calcScore dice sheet =
            calcScore dice |> setField sheet
    in
        let
            numBox n setter =
                Available (boxUpdate setter (sumDie n))

            fBox f setter =
                Available (boxUpdate setter f)
        in
            { aces = numBox 1 (\sh sc -> { sh | aces = Played sc })
            , twos = numBox 2 (\sh sc -> { sh | twos = Played sc })
            , threes = numBox 3 (\sh sc -> { sh | threes = Played sc })
            , fours = numBox 4 (\sh sc -> { sh | fours = Played sc })
            , fives = numBox 5 (\sh sc -> { sh | fives = Played sc })
            , sixes = numBox 6 (\sh sc -> { sh | sixes = Played sc })
            , threeOfAKind = fBox (ofAKind 3) (\sh sc -> { sh | threeOfAKind = Played sc })
            , fourOfAKind = fBox (ofAKind 4) (\sh sc -> { sh | fourOfAKind = Played sc })
            , fullHouse = fBox fullHouse (\sh sc -> { sh | fullHouse = Played sc })
            , smallStraight = fBox (straight 4 25) (\sh sc -> { sh | smallStraight = Played sc })
            , largeStraight = fBox (straight 5 35) (\sh sc -> { sh | largeStraight = Played sc })
            , yahtzee = fBox yahtzee (\sh sc -> { sh | yahtzee = Played sc })
            , chance = fBox chance (\sh sc -> { sh | chance = Played sc })
            }


sumDie : Int -> Dice -> Score
sumDie die dice =
    dice |> Dice.asNumberList |> filter (\d -> d == die) |> sum


ofAKind : Int -> Dice -> Score
ofAKind n dice =
    case nOfAKindHelper n (Dice.asNumberList dice) Dict.empty of
        Just x ->
            dice |> Dice.asNumberList |> sum

        Nothing ->
            0


incVal x =
    case x of
        Nothing ->
            Just 1

        Just v ->
            Just (v + 1)


nOfAKindHelper n dice counter =
    case dice of
        [] ->
            Nothing

        die :: rest ->
            let
                newCounter =
                    Dict.update die incVal counter

                count =
                    Dict.get die newCounter |> Maybe.withDefault 0
            in
                if count >= n then
                    Just die
                else
                    nOfAKindHelper n rest newCounter


sumScores xs =
    case xs of
        [] ->
            0

        (Played x) :: rest ->
            x + sumScores rest

        _ :: rest ->
            sumScores rest


upperTotal sheet =
    sumScores
        [ sheet.aces
        , sheet.twos
        , sheet.threes
        , sheet.fours
        , sheet.fives
        , sheet.sixes
        ]


lowerTotal sheet =
    sumScores
        [ sheet.threeOfAKind
        , sheet.fourOfAKind
        , sheet.fullHouse
        , sheet.smallStraight
        , sheet.largeStraight
        , sheet.yahtzee
        , sheet.chance
        ]


upperBonus sheet =
    if (upperTotal sheet >= 62) then
        35
    else
        0


totalScore sheet =
    upperTotal sheet + upperBonus sheet + lowerTotal sheet


yahtzee dice =
    case nOfAKindHelper 5 (Dice.asNumberList dice) Dict.empty of
        Just _ ->
            50

        _ ->
            0


straight size pts dice =
    if (dice |> Dice.asNumberList |> sort |> (countSequential 1 0)) >= size then
        pts
    else
        0


countSequential current longest dice =
    case dice of
        head :: neck :: rest ->
            let
                newCurrent =
                    case (neck - head) of
                        1 ->
                            current + 1

                        0 ->
                            current

                        _ ->
                            1
            in
                countSequential newCurrent (max newCurrent longest) (neck :: rest)

        [ _ ] ->
            longest

        [] ->
            longest


fullHouse dice =
    case dice |> Dice.asNumberList |> sort |> count |> Dict.values of
        [ 2, 3 ] ->
            25

        [ 3, 2 ] ->
            25

        -- I think a yahtzee is technically a full house.
        [ 5 ] ->
            25

        _ ->
            0


chance dice =
    dice |> Dice.asNumberList |> sum


count items =
    foldr (\v d -> Dict.update v incVal d) Dict.empty items
