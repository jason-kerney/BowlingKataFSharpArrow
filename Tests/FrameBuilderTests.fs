module Bowling.Tests.FrameBuilderTests
open Bowling.Lib
open Archer.Core
open Archer

type SpareTestDatum =
    { FirstPins: int; SecondPins: int; ThirdPins: int }

let bowlSpareDatum (datum: SpareTestDatum) =
    {
        Input = datum
        Expected = None
        NameGenerator = fun input -> sprintf "(PartialFrame %d, [Spare (%d, %d, %d)]) when bowling %d on (PartialSpare (%d, %d))" input.ThirdPins input.FirstPins input.SecondPins input.ThirdPins input.ThirdPins input.FirstPins input.SecondPins
    }

let frameBuilderTestFeature =
    rootFeature
    |> Sub.Feature (
        "Frame Builder",
        TestTags [
            Category "FrameBuilder"
        ]
    )

type KeepsPreviousTestDatum =
    {
        PreviousState: IncompleteFrame
        PreviousFrames: Frame list
        Pins: int
        NewFrame: Frame option
    }

let bowlKeepsPreviousDatum(datum: KeepsPreviousTestDatum) =
    let expected =
        match datum.NewFrame with
        | Some frame -> frame :: datum.PreviousFrames
        | None -> datum.PreviousFrames
    {
        Input = datum
        Expected = expected
        NameGenerator = fun input -> sprintf "when given %A and %A when bowling %d then return %A" input.PreviousState input.PreviousFrames input.Pins expected
    }


let ``should start with an empty frame`` = 
    frameBuilderTestFeature.Test (
        fun _ ->
            FrameBuilder.create ()
            |> Should.BeEqualTo { InProgress = EmptyFrame; Finished = [] }
    )

let ``should return (EmptyFrame, [Basic %A]) when bowling pins on (Partial previousPins, []) and previousPins + pins <> 10`` =
    frameBuilderTestFeature.Test (
        Data [ (1, 4); (2, 7) ],
        fun (previousPins, pins) ->
            FrameBuilder.bowl pins { InProgress = PartialFrame previousPins; Finished = [] }
            |> Should.BeEqualTo { InProgress = EmptyFrame; Finished = [Basic (previousPins, pins)] }
    )

let ``should return (Partial %d, []) when bowling N on (EmptyFrame, [])`` =
    frameBuilderTestFeature.Test (
        Data [2; 5],
        fun pins _ ->
            FrameBuilder.bowl pins { InProgress = EmptyFrame; Finished = [] }
            |> Should.BeEqualTo { InProgress = PartialFrame pins; Finished = [] }
    )

let ``should return (EmptyFrame, previousFrames plus Basic (prev, pins)) when bowling pins on (PartialFrame prev, previousFrames) and prev + pins <> 10`` =
    frameBuilderTestFeature.Test (
        fun _ ->
            let previousFrames = [Basic (2, 3); Basic (4, 5)]
            let prev, pins = 1, 4
            let result = FrameBuilder.bowl pins { InProgress = PartialFrame prev; Finished = previousFrames }
            let expected = { InProgress = EmptyFrame; Finished = Basic (prev, pins) :: previousFrames }
            result |> Should.BeEqualTo expected
    )

let ``should return (PartialSpare %A, []) when bowling pins on (PartialFrame prev, []) and prev + pins = 10`` =
    frameBuilderTestFeature.Test (
        Data [ (7, 3); (5, 5) ],
        fun (prev, pins) _ ->
            FrameBuilder.bowl pins { InProgress = PartialFrame prev; Finished = [] }
            |> Should.BeEqualTo { InProgress = PartialSpare (prev, pins); Finished = [] }
    )


let ``should not change state if bowling more than 10 on PartialFrame`` =
    frameBuilderTestFeature.Test (
        fun _ ->
            let state = { InProgress = PartialFrame 8; Finished = [Basic (2, 3)] }
            let result = FrameBuilder.bowl 3 state
            result |> Should.BeEqualTo state
    )


let ``should return %O`` =

    frameBuilderTestFeature.Test (
        Data [
            bowlSpareDatum({ FirstPins = 5; SecondPins = 5; ThirdPins = 2 })
            bowlSpareDatum({ FirstPins = 3; SecondPins = 7; ThirdPins = 8 })
        ],
        fun { Input = data } ->
            let result = FrameBuilder.bowl data.ThirdPins { InProgress = PartialSpare (data.FirstPins, data.SecondPins); Finished = [] }
            result |> Should.BeEqualTo { InProgress = PartialFrame data.ThirdPins; Finished = [Spare (data.FirstPins, data.SecondPins, data.ThirdPins)] }
    )


let ``should return previous frames %O `` =
    frameBuilderTestFeature.Test (
        Data [
            bowlKeepsPreviousDatum({
                PreviousState = EmptyStrike
                PreviousFrames = [Basic (1, 2)]
                Pins = 7
                NewFrame = None
            })
            bowlKeepsPreviousDatum({
                PreviousState = EmptyFrame
                PreviousFrames = [Basic (1, 2)]
                Pins = 10
                NewFrame = None
            })
            bowlKeepsPreviousDatum({ 
                PreviousState = EmptyFrame
                PreviousFrames = []
                Pins = 5
                NewFrame = None
            })
            bowlKeepsPreviousDatum({
                PreviousState = PartialFrame 3
                PreviousFrames = [Basic (1, 2)]
                Pins = 6
                NewFrame = Some (Basic (3, 6))
            })
            bowlKeepsPreviousDatum({
                PreviousState = PartialFrame 4
                PreviousFrames = [Basic (1, 2); Basic (3, 4)]
                Pins = 5
                NewFrame = Some (Basic (4, 5))
            })
            bowlKeepsPreviousDatum({
                PreviousState = PartialSpare (1, 9)
                PreviousFrames = []
                Pins = 5
                NewFrame = Some (Spare (1, 9, 5))
            })
            bowlKeepsPreviousDatum({
                PreviousState = PartialSpare (1, 9)
                PreviousFrames = []
                Pins = 5
                NewFrame = Some (Spare (1, 9, 5))
            })
            bowlKeepsPreviousDatum({ 
                PreviousState = PartialSpare (2, 3)
                PreviousFrames = [Basic (1, 2)]
                Pins = 5
                NewFrame = Some (Spare (2, 3, 5))
            })
            bowlKeepsPreviousDatum({
                PreviousState = PartialFrame 4
                PreviousFrames = [Basic (1, 2); Basic (3, 4)]
                Pins = 5
                NewFrame = Some (Basic (4, 5))
            })
        ],
        fun { Input = testData; Expected = expected } ->
            let previousFrames, pins, previousState = testData.PreviousFrames, testData.Pins, testData.PreviousState
            let result = 
                FrameBuilder.bowl pins { InProgress = previousState; Finished = previousFrames }
            result.Finished |> Should.BeEqualTo expected
    )

let ``should return (EmptyStrike, []) when bowling 10 on (EmptyFrame, [])`` =
    frameBuilderTestFeature.Test (
        fun _ ->
            let result = FrameBuilder.bowl 10 { InProgress = EmptyFrame; Finished = [] }
            result |> Should.BeEqualTo { InProgress = EmptyStrike; Finished = [] }
    )

let ``should return (PartialStrike pins, []) when bowling %d on (EmptyStrike, [])`` =
    frameBuilderTestFeature.Test (
        Data [5; 7],
        fun pins _ ->
            let result = FrameBuilder.bowl pins { InProgress = EmptyStrike; Finished = [] }
            result |> Should.BeEqualTo { InProgress = PartialStrike pins; Finished = [] }
    )

let ``should not change state when bowling 11 on (EmptyFrame, frames)`` =
    frameBuilderTestFeature.Test (
        fun _ ->
            let state = { InProgress = EmptyFrame; Finished = [Basic (1, 2)] }
            let result = FrameBuilder.bowl 11 state
            result |> Should.BeEqualTo state
    )

let ``should handle PartialStrike correctly for various inputs and returns %A`` =
    frameBuilderTestFeature.Test (
        Data [ 
            (10, 5, { InProgress = PartialStrike 5; Finished = [Strike (10, 5)] })
            (2, 2, { InProgress = EmptyFrame; Finished = [Basic (2, 2); Strike (2, 2)] }) 
        ],
        fun (prevPins, pins, expected) _ ->
            let result = FrameBuilder.bowl pins { InProgress = PartialStrike prevPins; Finished = [] }
            result |> Should.BeEqualTo expected
    )



let ``should return correct score for Frame values`` =
    frameBuilderTestFeature.Test (
        Data [
            (Basic (4, 5), 9)
            (Basic (0, 0), 0)
            (Basic (1, 2), 3)
            (Spare (6, 4, 2), 12)
            (Spare (0, 10, 0), 10)
            (Spare (5, 5, 10), 20)
            (Strike (10, 5), 25)
            (Strike (0, 0), 10)
            (Strike (7, 3), 20)
        ],
        fun (frame, expected) _ ->
            let result = FrameBuilder.score frame
            result |> Should.BeEqualTo expected
    )

let ``Test Cases`` = frameBuilderTestFeature.GetTests ()