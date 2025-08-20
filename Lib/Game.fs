// ...existing code...

module Bowling.Lib.Game
open Bowling.Lib
open Bowling.Lib.ScorePad
open System.Collections.Generic
open System.Collections.ObjectModel

type Game = {
    States: IReadOnlyDictionary<Player, State>
    CurrentIndex: int
}

let getFullState (_: Game) : (Player * int * int) list =
    failwith "Not implemented"

let create players =
    let states = Dictionary<Player, State>()
    for p in players do
        states.Add(p, { InProgress = EmptyFrame; Finished = [] })
    { States = ReadOnlyDictionary(states) :> IReadOnlyDictionary<Player, State>; CurrentIndex = 0 }

let getPlayers (game: Game) =
    game.States.Keys |> Seq.toList

let getScores (game: Game) =
    getPlayers game
    |> List.map (fun p ->
        let state = game.States.[p]
        let score = state.Finished |> FrameBuilder.getTotalScore
        (p, score)
    )

let getCurrentPlayer (game: Game) =
    let players = getPlayers game
    match players with
    | [] -> failwith "No players in game"
    | _ ->
        let idx = game.CurrentIndex % players.Length
        players.[idx]

let bowl pins (game: Game) =
    let players = getPlayers game
    let numPlayers = players.Length
    match players with
    | [] -> game
    | _ ->
        let rec findNext idx count =
            if count >= numPlayers then idx // fallback: all finished
            else
                let p = players.[idx % numPlayers]
                let state = game.States.[p]
                if isInProgress state then idx % numPlayers
                else findNext (idx + 1) (count + 1)

        let idx = game.CurrentIndex % numPlayers
        let current = players.[idx]
        let state = game.States.[current]
        let newState = FrameBuilder.bowl pins state
        let newStates = Dictionary<Player, State>(game.States)
        newStates.[current] <- newState
        let readOnlyStates = ReadOnlyDictionary(newStates) :> IReadOnlyDictionary<Player, State>
        let shouldAdvance =
            match newState.InProgress with
            | EmptyFrame | EmptyStrike | PartialStrike 10 | PartialSpare _ -> true
            | _ -> false
        let nextIndex =
            if shouldAdvance then findNext (idx + 1) 0 else idx
        { States = readOnlyStates; CurrentIndex = nextIndex }
