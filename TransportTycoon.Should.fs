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
            testCase "not load waiting transport when no cargo stocked" (fun () ->
                let waitingTransportWithoutCargosStocked =
                    {
                        StockedCargos = Map.ofList []
                        Transports = [ Truck WaitingAt Factory ]
                    }
                test <@ load waitingTransportWithoutCargosStocked = 
                            waitingTransportWithoutCargosStocked @>
            )
            testCase "not load when transport not waiting" (fun () ->
                let inTransitTransport =
                    {
                        StockedCargos = Map.ofList []
                        Transports = [ Truck InTransitTo (Port, Some WarehouseA, 1) ]
                    }
                test <@ load inTransitTransport = inTransitTransport @>
            )
        ]

        testList "Move should" [
            testCase "move in transit transports" (fun () ->
                let inTransitTransports =
                    {
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck InTransitTo (WarehouseB, Some WarehouseB, 4)
                            Boat InTransitTo (WarehouseA, Some WarehouseA, 4)
                        ]
                    }
                test <@ move inTransitTransports = 
                            {
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck InTransitTo (WarehouseB, Some WarehouseB, 3)
                                    Boat InTransitTo (WarehouseA, Some WarehouseA, 3)
                                ]
                            } @>
            )
            testCase "be unloading when in transit with a cargo and only 1 remaining hour" (fun () ->
                let inTransitTransports =
                    {
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck InTransitTo (Port, Some WarehouseA, 1)
                            Boat InTransitTo (WarehouseA, Some WarehouseA, 1)
                        ]
                    }
                test <@ move inTransitTransports = 
                            {
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck UnloadingAt (Port, WarehouseA)
                                    Boat UnloadingAt (WarehouseA, WarehouseA)
                                ]
                            } @>
            )
            testCase "be waiting when in transit without any cargo and only 1 remaining hour" (fun () ->
                let inTransitTransports =
                    {
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck InTransitTo (Factory, None, 1)
                            Boat InTransitTo (Port, None, 1)
                        ]
                    }
                test <@ move inTransitTransports = 
                            {
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck WaitingAt Factory
                                    Boat WaitingAt Port
                                ]
                            } @>
            )
            testCase "not move when not in transit" (fun () ->
                let notInTransitTransports =
                    {
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck WaitingAt Factory
                            Boat UnloadingAt (Port, WarehouseA)
                        ]
                    }
                test <@ move notInTransitTransports = notInTransitTransports @>
            )
        ]
    ]
