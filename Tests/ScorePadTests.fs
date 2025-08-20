module Bowling.Tests.ScorePadTests
open Bowling.Lib
open Bowling.Lib.ScorePad

open Archer.Arrows
open Archer

let scorePadBowlDatum (pins: int, frame: IncompleteFrame) =
    {
        Input = pins
        Expected = frame
        NameGenerator = fun _ -> sprintf "when bowl of %d then current state will be %A" pins frame
    }

let scorePadScoreDatum (pins: int list, expectedScore: int) =
    {
        Input = pins
        Expected = expectedScore
        NameGenerator = fun input -> sprintf "when bowling %A then score should be %d" input expectedScore
    }

let scorePadTestFeature =
    rootFeature
    |> Sub.Feature (
        "Score Pad",
        TestTags [
            Category "ScorePad"
        ]
    )

let ``should return 0 when getting score from a newly created scorepad`` =
    scorePadTestFeature.Test (
        fun _ ->
            let scorePad = ScorePad.create ()
            let score = ScorePad.getScore scorePad
            score |> Should.BeEqualTo 0
    )

let ``should return true when getting if the game is inprogress from a newly created scorepad`` =
    scorePadTestFeature.Test (
        fun _ ->
            let scorePad = ScorePad.create ()
            let isInProgress = ScorePad.isInProgress scorePad
            isInProgress |> Should.BeTrue
    )

let ``should %O`` =
    scorePadTestFeature.Test (
        Data [
            scorePadBowlDatum(0, PartialFrame 0)
            scorePadBowlDatum(1, PartialFrame 1)
            scorePadBowlDatum(10, EmptyStrike)
        ],
        fun { Input=pins; Expected=expected } ->
            let scorePad = ScorePad.create ()
            let { InProgress=updatedState } = scorePad |> ScorePad.bowl pins
            updatedState |> Should.BeEqualTo expected
    )

let ``score should %O`` =
    scorePadTestFeature.Test (
        Data [
            scorePadScoreDatum([0], 0)
            scorePadScoreDatum([1], 0)
            scorePadScoreDatum([10], 0)
            scorePadScoreDatum([10; 2], 0)
            scorePadScoreDatum([5; 5], 0)
            scorePadScoreDatum([1; 2], 3)
            scorePadScoreDatum([5; 5; 2], 12)
            scorePadScoreDatum([10; 2; 7], 19 + 9)
            scorePadScoreDatum([10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10], 300)
            scorePadScoreDatum([10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10], 300)
        ],
        fun { Input=pins; Expected=expected } ->
            pins
            |> List.fold (fun scorePad pin -> ScorePad.bowl pin scorePad) (ScorePad.create ())
            |> ScorePad.getScore
            |> Should.BeEqualTo expected
    )

let ``should not be in progress after bowling 10 frames`` =
    scorePadTestFeature.Test (
        fun _ ->
            let scorePad = ScorePad.create ()
            let finalState = 
                [1..20]
                |> List.fold (fun pad _ -> ScorePad.bowl 1 pad) scorePad
            let isInProgress = ScorePad.isInProgress finalState
            isInProgress |> Should.BeFalse
    )

let ``Test Cases`` = scorePadTestFeature.GetTests ()

