module Game exposing (..)

import Dice exposing (..)
import Html exposing (Html, button, div, table, td, text, th, tr)
import Html.Events exposing (onClick)
import Random
import Scoresheet exposing (..)
import String exposing (fromInt)


type Roll
    = FirstRoll
    | SecondRoll
    | LastRoll


type GameState
    = GameState Scoresheet Dice Roll Random.Seed


type Action
    = UpdateScorecard (Dice -> Scoresheet -> Scoresheet)
    | NewGame
    | Roll
    | UpdateDice Dice


initialState seed =
    let
        ( initialDice, seedNext ) =
            rollAll (Random.initialSeed seed)
    in
    GameState newScoresheet initialDice FirstRoll seedNext


newGameButton : Html.Html Action
newGameButton =
    button [ onClick NewGame ] [ text "New Game" ]


rollDiceButton (GameState _ _ roll _) =
    button [ onClick Roll ] [ text (rollButtonText roll) ]


rollButtonText roll =
    case roll of
        FirstRoll ->
            "Roll Again"

        SecondRoll ->
            "Last Roll"

        LastRoll ->
            "No Rolls Left"


update : Action -> GameState -> GameState
update action (GameState prevScoresheet dice roll seed) =
    let
        newstate =
            case action of
                Roll ->
                    case roll of
                        FirstRoll ->
                            let
                                ( dice2, seed2 ) =
                                    rollUnheld dice seed
                            in
                            GameState prevScoresheet dice2 SecondRoll seed2

                        SecondRoll ->
                            let
                                ( dice2, seed2 ) =
                                    rollUnheld dice seed
                            in
                            GameState prevScoresheet dice2 LastRoll seed2

                        LastRoll ->
                            GameState prevScoresheet dice LastRoll seed

                UpdateScorecard setter ->
                    let
                        ( dice2, seed2 ) =
                            rollAll seed
                    in
                    GameState (setter dice prevScoresheet) dice2 FirstRoll seed2

                NewGame ->
                    let
                        ( dice2, seed2 ) =
                            rollAll seed
                    in
                    GameState newScoresheet dice2 FirstRoll seed2

                UpdateDice newDice ->
                    GameState prevScoresheet newDice roll seed
    in
    newstate


scoresheet (GameState sb dice roll seed) =
    let
        textCol title =
            tr [] [ th [] [ text title ] ]

        boxColumn title s =
            let
                score box =
                    case box of
                        Played points ->
                            text <| fromInt points

                        Available setter ->
                            button [ onClick (UpdateScorecard setter) ] [ text " - " ]
            in
            tr [] [ td [] [ text title ], td [] [ score s ] ]
    in
    table []
        [ textCol "Upper Section"
        , boxColumn "Aces" sb.aces
        , boxColumn "Twos" sb.twos
        , boxColumn "Threes" sb.threes
        , boxColumn "Four" sb.fours
        , boxColumn "Fives" sb.fives
        , boxColumn "Sixes" sb.sixes
        , boxColumn "Bonus" (Played (upperBonus sb))
        , textCol "Lower Section"
        , boxColumn "3 of a kind" sb.threeOfAKind
        , boxColumn "4 of a kind" sb.fourOfAKind
        , boxColumn "Full House" sb.fullHouse
        , boxColumn "Small Straight" sb.smallStraight
        , boxColumn "Large Straight" sb.largeStraight
        , boxColumn "Yahtzee" sb.yahtzee
        , boxColumn "Chance" sb.chance
        , boxColumn "Total" (Played (totalScore sb))
        ]


view ((GameState sheet dice roll seed) as gamestate) =
    div [] [ Dice.view UpdateDice dice, scoresheet gamestate, rollDiceButton gamestate, newGameButton ]
