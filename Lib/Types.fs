
namespace Bowling.Lib

type Player = Player of string

type IncompleteFrame =
    | EmptyFrame
    | PartialFrame of int
    | PartialSpare of int * int
    | EmptyStrike
    | PartialStrike of int

type Frame =
    | Basic of int * int
    | Spare of int * int * int
    | Strike of int * int

type State = {
    InProgress: IncompleteFrame
    Finished: Frame list
}