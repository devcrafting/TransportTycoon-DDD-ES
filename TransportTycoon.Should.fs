module TransportTycoonShould

open Expecto
open Swensen.Unquote
open TransportTycoon

[<Tests>]
let should =
    testList "All tests" [
        testList "Compute how long it takes to deliver should" [
            testCase "return 0 when no cargos" (fun () ->
                test <@ computeHowLongItTakesToDeliver [] = 0 @>
            )
        ]

        testList "Load should" [
            testCase "load waiting transport when there is cargo stocked at waiting location" (fun () ->
                let waitingTransportWithCargoStockedAtWaitingLocation =
                    {
                        StockedCargos = Map.ofList [(Factory, [WarehouseA]); (Port, [WarehouseA])]
                        Transports = [ Truck WaitingAt Factory; Boat WaitingAt Port ]
                    }
                test <@ load waitingTransportWithCargoStockedAtWaitingLocation = 
                            {
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck InTransitTo (Port, Some WarehouseA, 1)
                                    Boat InTransitTo (WarehouseA, Some WarehouseA, 4)
                                ]
                            } @>
            )
            testCase "do not load waiting transport when no cargo stocked" (fun () ->
                let waitingTransportWithoutCargosStocked =
                    {
                        StockedCargos = Map.ofList []
                        Transports = [ Truck WaitingAt Factory ]
                    }
                test <@ load waitingTransportWithoutCargosStocked = 
                            waitingTransportWithoutCargosStocked @>
            )
            testCase "do not load when transport not waiting" (fun () ->
                let inTransitTransport =
                    {
                        StockedCargos = Map.ofList []
                        Transports = [ Truck InTransitTo (Port, Some WarehouseA, 1) ]
                    }
                test <@ load inTransitTransport = inTransitTransport @>
            )
        ]
    ]
