open Expecto
open TransportTycoon.Domain
open TransportTycoon.Infra

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        let request =
            argv.[0]
            |> Seq.map (function
                | 'A' -> WarehouseA
                | 'B' -> WarehouseB)
            |> Seq.toList
        let finalState = 
            computeHowLongItTakesToDeliver request
                [ Truck 0 WaitingAt Factory; Truck 1 WaitingAt Factory; Boat 0 WaitingAt Port ]
        (snd finalState).History |> writeLogs
        0
    else
        runTestsInAssembly defaultConfig argv
