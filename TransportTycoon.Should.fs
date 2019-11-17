module TransportTycoonShould

open Expecto
open Swensen.Unquote
open TransportTycoon

[<Tests>]
let should =
    testList "All tests" [
        testList "Compute how long it takes to deliver should" [
            let withTransports = [ Truck WaitingAt Factory; Truck WaitingAt Factory; Boat WaitingAt Port ]
            testCase "return 0 when no cargos" (fun () ->
                test <@ computeHowLongItTakesToDeliver [] withTransports |> fst = 0 @>
            )
            testCase "return 5 when one cargo A" (fun () ->
                test <@ computeHowLongItTakesToDeliver [WarehouseA] withTransports |> fst = 5 @>
            )
            testCase "return 5 when one cargo AB" (fun () ->
                test <@ computeHowLongItTakesToDeliver [WarehouseA; WarehouseB] withTransports |> fst = 5 @>
            )
            testCase "return 5 when one cargo BB" (fun () ->
                test <@ computeHowLongItTakesToDeliver [WarehouseB; WarehouseB] withTransports |> fst = 5 @>
            )
            testCase "return 7 when one cargo ABB" (fun () ->
                test <@ computeHowLongItTakesToDeliver [WarehouseA; WarehouseB; WarehouseB] withTransports |> fst = 7 @>
            )
            testCase "return 13 when one cargo AAB" (fun () ->
                test <@ computeHowLongItTakesToDeliver [WarehouseA; WarehouseA; WarehouseB] withTransports |> fst = 13 @>
            )
        ]

        let world = { StockedCargos = Map.empty; Transports = []; History = [] }

        testList "Load should" [
            testCase "load waiting transport when there is cargo stocked at waiting location" (fun () ->
                let waitingTransportWithCargoStockedAtWaitingLocation =
                    { world with
                        StockedCargos = Map.ofList [(Factory, [WarehouseA]); (Port, [WarehouseA])]
                        Transports = [ Truck WaitingAt Factory; Boat WaitingAt Port ]
                    }
                let currentTime = System.Random().Next()
                test <@ load currentTime waitingTransportWithCargoStockedAtWaitingLocation = 
                            {
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck InTransitTo (Port, Some WarehouseA, 1)
                                    Boat InTransitTo (WarehouseA, Some WarehouseA, 4)
                                ]
                                History = [
                                    Departed (Port, {
                                        Time = currentTime
                                        Kind = Transport.Truck
                                        Location = Factory
                                        Cargo = { Destination = WarehouseA }
                                    })
                                    Departed (WarehouseA, {
                                        Time = currentTime
                                        Kind = Transport.Boat
                                        Location = Port
                                        Cargo = { Destination = WarehouseA }
                                    })
                                ]
                            } @>
            )
            testCase "not load waiting transport when no cargo stocked" (fun () ->
                let waitingTransportWithoutCargosStocked =
                    { world with
                        StockedCargos = Map.ofList []
                        Transports = [ Truck WaitingAt Factory ]
                    }
                test <@ load 0 waitingTransportWithoutCargosStocked = 
                            waitingTransportWithoutCargosStocked @>
            )
            testCase "not load when transport not waiting" (fun () ->
                let inTransitTransport =
                    { world with
                        StockedCargos = Map.ofList []
                        Transports = [ Truck InTransitTo (Port, Some WarehouseA, 1) ]
                    }
                test <@ load 0 inTransitTransport = inTransitTransport @>
            )
        ]

        testList "Move should" [
            testCase "move in transit transports" (fun () ->
                let inTransitTransports =
                    { world with
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck InTransitTo (WarehouseB, Some WarehouseB, 4)
                            Boat InTransitTo (WarehouseA, Some WarehouseA, 4)
                        ]
                    }
                test <@ move inTransitTransports = 
                            { world with
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck InTransitTo (WarehouseB, Some WarehouseB, 3)
                                    Boat InTransitTo (WarehouseA, Some WarehouseA, 3)
                                ]
                            } @>
            )
            testCase "be unloading when in transit with a cargo and only 1 remaining hour" (fun () ->
                let inTransitTransports =
                    { world with
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck InTransitTo (Port, Some WarehouseA, 1)
                            Boat InTransitTo (WarehouseA, Some WarehouseA, 1)
                        ]
                    }
                test <@ move inTransitTransports = 
                            { world with
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck UnloadingAt (Port, WarehouseA)
                                    Boat UnloadingAt (WarehouseA, WarehouseA)
                                ]
                            } @>
            )
            testCase "be waiting when in transit without any cargo and only 1 remaining hour" (fun () ->
                let inTransitTransports =
                    { world with
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck InTransitTo (Factory, None, 1)
                            Boat InTransitTo (Port, None, 1)
                        ]
                    }
                test <@ move inTransitTransports = 
                            { world with
                                StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                                Transports = [
                                    Truck WaitingAt Factory
                                    Boat WaitingAt Port
                                ]
                            } @>
            )
            testCase "not move when not in transit" (fun () ->
                let notInTransitTransports =
                    { world with
                        StockedCargos = Map.ofList [(Factory, []); (Port, [])]
                        Transports = [
                            Truck WaitingAt Factory
                            Boat UnloadingAt (Port, WarehouseA)
                        ]
                    }
                test <@ move notInTransitTransports = notInTransitTransports @>
            )
        ]
        
        testList "Unload should" [
            testCase "unload cargo in location stock and go back without cargos" (fun () ->
                let unloadingTranports =
                    { world with
                        StockedCargos = Map.ofList []
                        Transports = [
                            Truck UnloadingAt (WarehouseB, WarehouseB)
                            Boat UnloadingAt (WarehouseA, WarehouseA)
                        ]
                    }
                test <@ unload unloadingTranports = 
                            { world with
                                StockedCargos = Map.ofList [(WarehouseA, [WarehouseA]); (WarehouseB, [WarehouseB])]
                                Transports = [
                                    Truck InTransitTo (Factory, None, 5)
                                    Boat InTransitTo (Port, None, 4)
                                ]
                            } @>
            )
            testCase "not unload when not prepared for unloading" (fun () ->
                let notUnloadingTranports =
                    { world with
                        StockedCargos = Map.ofList []
                        Transports = [
                            Truck WaitingAt Factory
                            Boat InTransitTo (Port, None, 4)
                        ]
                    }
                test <@ unload notUnloadingTranports = notUnloadingTranports @>
            )
        ]
    ]
