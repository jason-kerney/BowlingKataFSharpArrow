[<AutoOpen>]
module Bowling.Tests.Framework

open Archer.Arrows
open Archer.ApprovalsSupport
open ApprovalTests

let rootFeature = Arrow.NewFeature (
    "Bowling.Tests", 
    "",
    Setup (fun _ ->
        [
            Searching
                |> findFirstReporter<Reporters.DiffReporter>
                |> findFirstReporter<Reporters.WinMergeReporter>
                |> findFirstReporter<Reporters.InlineTextReporter>
                |> findFirstReporter<Reporters.AllFailingTestsClipboardReporter>
                |> unWrapReporter
                
            Reporters.ClipboardReporter () :> Core.IApprovalFailureReporter

            Reporters.QuietReporter () :> Core.IApprovalFailureReporter
        ]
        |> buildReporter
        |> Ok
        )
    )