[<AutoOpen>]
module TestTypes

open Bowling.Lib

type TestDatum<'input, 'expected> =
    {
        Input: 'input
        Expected: 'expected
        NameGenerator: 'input -> string
    }
    override this.ToString (): string = 
            this.Input |> this.NameGenerator