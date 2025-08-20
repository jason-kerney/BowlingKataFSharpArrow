module Bowling.Lib.ScorePad
open Bowling.Lib

let isInProgress (pad: State) =
    pad.Finished.Length < 10

let create () = { InProgress = EmptyFrame; Finished = [] }

let bowl pins (pad: State) =
    if not (isInProgress pad) then
        pad
    else
        FrameBuilder.bowl pins pad

let getScore (pad: State) =
    pad.Finished
    |> FrameBuilder.getTotalScore