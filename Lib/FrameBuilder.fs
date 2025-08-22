module Bowling.Lib.FrameBuilder
open Bowling.Lib

let score = function
    | Basic (firstRoll, secondRoll) -> firstRoll + secondRoll
    | Spare (_, _, bonusRoll) -> 10 + bonusRoll
    | Strike (nextRoll, bonusRoll) -> 10 + nextRoll + bonusRoll

let getTotalScore (frames: Frame list) =
    frames 
    |> List.sumBy score

let create () = { InProgress = EmptyFrame; Finished = [] }

let rec bowlAll (pinSets: int list) ({ InProgress = incomplete; Finished = frames } as state) =
    match pinSets with
    | [] -> state
    | pins :: rest ->
        match (incomplete, pins) with
        | (EmptyFrame, pins) when 10 < pins -> 
            state
        | (EmptyFrame, 10) -> 
            { InProgress = EmptyStrike; Finished = frames } |> bowlAll rest
        | (EmptyFrame, pins) -> 
            { InProgress = PartialFrame pins; Finished = frames } |> bowlAll rest
        | (EmptyStrike, pins) -> 
            { InProgress = PartialStrike pins; Finished = frames } |> bowlAll rest
        | (PartialStrike previousPins, pins) -> 
            { InProgress = EmptyFrame; Finished = Strike (previousPins, pins)::frames } |> bowlAll (previousPins::pins::rest)
        | (PartialFrame previousPins, pins) when 10 < (previousPins + pins) -> 
            state
        | (PartialFrame previousPins, pins) when 10 = (previousPins + pins) -> 
            { InProgress = PartialSpare (previousPins, pins); Finished = frames } |> bowlAll rest
        | (PartialFrame previousPins, pins) -> 
            { InProgress = EmptyFrame; Finished = Basic (previousPins, pins)::frames } |> bowlAll rest
        | (PartialSpare (prev, prev2), pins) -> 
            { InProgress = EmptyFrame; Finished = Spare (prev, prev2, pins)::frames } |> bowlAll (pins::rest)

and bowl pins = bowlAll [pins]
