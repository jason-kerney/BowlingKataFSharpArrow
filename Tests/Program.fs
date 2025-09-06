open Archer.Runner
open Archer
open Archer.Types.InternalTypes
open Archer.Types.InternalTypes.RunnerTypes
open Archer.Reporting.Summaries
open Bowling.Tests
open MicroLang.Lang

let runner = runnerFactory.Runner ()

runner.RunnerLifecycleEvent
|> Event.add (fun args ->
    match args with
    | RunnerStartExecution _ ->
        printfn ""
    | RunnerTestLifeCycle (test, testEventLifecycle, _) ->
        match testEventLifecycle with
        | TestEndExecution testExecutionResult ->
            match testExecutionResult with
            | TestExecutionResult TestSuccess -> ()
            | result ->
                let transformedResult = defaultTestExecutionResultSummaryTransformer result test
                printfn $"%s{transformedResult}"
            
        | _ -> ()
    | RunnerEndExecution ->
        printfn "\n"
)

runner
 |> addMany [
    FrameBuilderTests.``Test Cases``
    ScorePadTests.``Test Cases``
    GameTests.``Test Cases``
 ]
 |> runAndReport