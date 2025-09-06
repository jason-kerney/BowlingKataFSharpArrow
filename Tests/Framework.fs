[<AutoOpen>]
module Bowling.Tests.Framework

open Archer.Core
open Archer.ApprovalsSupport
open ApprovalTests

let rootFeature = FeatureFactory.NewFeature (
    "Bowling.Tests",
    ""
    )