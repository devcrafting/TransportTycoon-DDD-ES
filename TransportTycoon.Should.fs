module TransportTycoonShould

open Expecto
open Swensen.Unquote
open TransportTycoon

[<Tests>]
let should =
    testList "Compute how long it takes to deliver should" [
        testCase "return 0 when no cargos" (fun () ->
            test <@ computeHowLongItTakesToDeliver [] = 0 @>
        )
    ]
