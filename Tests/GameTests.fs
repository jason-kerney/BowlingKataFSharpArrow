module Bowling.Tests.GameTests
open Bowling.Lib
open Bowling.Lib.ScorePad

open Archer.Arrows
open Archer

let gameTestFeature =
    rootFeature
    |> Sub.Feature (
        "Game",
        TestTags [
            Category "Game"
        ]
    )


let ``should create a new game with players`` = gameTestFeature.Test (
    fun _ ->
        let player = Player "Jason"
        let game = Game.create [player]

        let scores = Game.getScores game

        scores |> Should.BeEqualTo [(player, 0)]
)

let ``should create a new game with 2 players`` = gameTestFeature.Test (
    fun _ ->
        let player1 = Player "Jason"
        let player2 = Player "Alex"
        let game = Game.create [player1; player2]

        let current = Game.getCurrentPlayer game

        current |> Should.BeEqualTo player1
)

let ``should update scores correctly after bowling`` = gameTestFeature.Test (
    fun _ ->
        let player1 = Player "Jason"
        let player2 = Player "Alex"
        
        let scores =
            [player1; player2]
            |> Game.create
            |> Game.bowl 1
            |> Game.bowl 2
            |> Game.getScores

        scores |> Should.BeEqualTo [(Player "Jason", 3); (Player "Alex", 0)]
)

let ``should have a current player of player 2 after bowling 3 and 4`` = gameTestFeature.Test (
    fun _ ->
        let player1 = Player "Jason"
        let player2 = Player "Alex"

        let game =
            [player1; player2]
            |> Game.create
            |> Game.bowl 3
            |> Game.bowl 4

        let current = Game.getCurrentPlayer game

        current |> Should.BeEqualTo player2
)

let ``should allow a player bowling only 1s to finish before a player bowling only strikes`` =
    gameTestFeature.Test (
        fun _ ->
            let player1 = Player "Jason"
            let player2 = Player "Alex"

            let game =
                [player1; player2]
                |> Game.create
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 1
                |> Game.bowl 10 // Player 2, 1
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 2
                |> Game.bowl 10 // Player 2, 2
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 3
                |> Game.bowl 10 // Player 2, 3
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 4
                |> Game.bowl 10 // Player 2, 4
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 5
                |> Game.bowl 10 // Player 2, 5
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 6
                |> Game.bowl 10 // Player 2, 6
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 7
                |> Game.bowl 10 // Player 2, 7
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 8
                |> Game.bowl 10 // Player 2, 8
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 9
                |> Game.bowl 10 // Player 2, 9
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 10
                |> Game.bowl 10 // Player 2, 10
                |> Game.bowl 10 // Player 2, 11

            let scores = Game.getCurrentPlayer game

            scores |> Should.BeEqualTo player2
    )

let ``should score a player bowling only 1s and a player bowling only strikes`` =
    gameTestFeature.Test (
        fun _ ->
            let player1 = Player "Bob"
            let player2 = Player "Susy"

            let game =
                [player1; player2]
                |> Game.create
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 1

                |> Game.bowl 10 // Player 2, 1

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 2

                |> Game.bowl 10 // Player 2, 2

                |> Game.bowl 1

                |> Game.bowl 1 // Player 1, 3

                |> Game.bowl 10 // Player 2, 3

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 4

                |> Game.bowl 10 // Player 2, 4

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 5

                |> Game.bowl 10 // Player 2, 5

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 6

                |> Game.bowl 10 // Player 2, 6

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 7

                |> Game.bowl 10 // Player 2, 7

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 8

                |> Game.bowl 10 // Player 2, 8

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 9

                |> Game.bowl 10 // Player 2, 9

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 10

                |> Game.bowl 10 // Player 2, 10
                |> Game.bowl 10 // Player 2, 11
                |> Game.bowl 10 // Player 2, 12

            let scores = Game.getScores game

            scores |> Should.BeEqualTo [player1, 20; player2, 300]
    )

//should score a game with 3 players
let ``should score a game with 3 players`` =
    gameTestFeature.Test (
        fun _ ->
            let player1 = Player "Alice"
            let player2 = Player "Bob"
            let player3 = Player "Charlie"

            let game =
                [player1; player2; player3]
                |> Game.create
                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 1

                |> Game.bowl 10 // Player 2, 1

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 1

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 2

                |> Game.bowl 10 // Player 2, 2

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 2

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 3

                |> Game.bowl 10 // Player 2, 3

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 3

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 4

                |> Game.bowl 10 // Player 2, 4

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 4

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 5

                |> Game.bowl 10 // Player 2, 5

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 5

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 6

                |> Game.bowl 10 // Player 2, 6

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 6

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 7

                |> Game.bowl 10 // Player 2, 7

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 7

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 8

                |> Game.bowl 10 // Player 2, 8

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 8

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 9

                |> Game.bowl 10 // Player 2, 9

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 9

                |> Game.bowl 1
                |> Game.bowl 1 // Player 1, 10

                |> Game.bowl 10 // Player 2, 10

                |> Game.bowl 2
                |> Game.bowl 2 // Player 3, 10

                |> Game.bowl 10 // Player 2, 11
                |> Game.bowl 10 // Player 2, 12

            let scores = Game.getScores game

            scores |> Should.BeEqualTo [(player1, 20); (player2, 300); (player3, 40)]
    )


let ``should show frame counts and scores when asked for the fullState`` = 
    gameTestFeature.Ignore (
    fun _ ->
        let player1 = Player "Alice"
        let player2 = Player "Bob"
        let player3 = Player "Charlie"

        let game =
            [player1; player2; player3]
            |> Game.create
            |> Game.bowl 1
            |> Game.bowl 1 // Player 1, 1

            |> Game.bowl 10 // Player 2, 1

            |> Game.bowl 2
            |> Game.bowl 2 // Player 3, 1

            |> Game.bowl 1
            |> Game.bowl 1 // Player 1, 2

            |> Game.bowl 10 // Player 2, 2

            |> Game.bowl 2
            |> Game.bowl 2 // Player 3, 2

        // This function does not exist yet
        let fullState = Game.getFullState game

        // Should return a list of (Player, score, frameCount)
        // This is a placeholder expectation
        fullState |> Should.BeEqualTo [(player1, 4, 2); (player2, 0, 0); (player3, 4, 2)]
)

let ``Test Cases`` = gameTestFeature.GetTests ()