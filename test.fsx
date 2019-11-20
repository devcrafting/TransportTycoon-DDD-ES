#load "TransportTycoon.Domain.fs"

open TransportTycoon.Domain

let world = { StockedCargos = Map.empty; Transports = []; History = [] }

let initialCargos = Map.empty |> Map.add Factory [WarehouseA; WarehouseB(*; WarehouseA*)]
let startingState = { world with
    StockedCargos = initialCargos
    Transports = [ Truck WaitingAt Factory; Truck WaitingAt Factory; Boat WaitingAt Port ]
}
let availableCargos = initialCargos |> Map.find Factory
let stockedCargosAfterUnloading =
    initialCargos |> Map.remove Factory
    //|> Map.add Factory (availableCargos |> List.skip 2)
    |> Map.add Port [WarehouseA]
let stateAfter1Hour = { world with
    StockedCargos = stockedCargosAfterUnloading 
    Transports = [ Truck InTransitTo (Factory, None, 1); Truck InTransitTo (WarehouseB, Some WarehouseB, 4); Boat WaitingAt Port ]
}
let stockedCargosAfterLoadingBoat = stockedCargosAfterUnloading |> Map.remove Port
let stateAfter2Hours = { world with
    StockedCargos = stockedCargosAfterLoadingBoat
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (WarehouseB, Some WarehouseB, 3); Boat InTransitTo (WarehouseA, Some WarehouseA, 3) ]
}
let stockedCargosAfterLoadingLastCargo = stockedCargosAfterLoadingBoat |> Map.remove Factory
let stateAfter3Hours = { world with
    StockedCargos = stockedCargosAfterLoadingLastCargo
    Transports = [ Truck WaitingAt Factory (*ArrivingIn (WarehouseA, Port)*); Truck InTransitTo (WarehouseB, Some WarehouseB, 2); Boat InTransitTo (WarehouseA, Some WarehouseA, 2) ]
}
let stockedCargosAfterBufferInPort = stockedCargosAfterLoadingLastCargo// |> Map.add Port [WarehouseA]
let stateAfter4Hours = { world with
    StockedCargos = stockedCargosAfterBufferInPort
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (WarehouseB, Some WarehouseB, 1); Boat InTransitTo (WarehouseA, Some WarehouseA, 1) ]
}
let stockedCargosAfterUnloadingInWarehouse =
    stockedCargosAfterBufferInPort
    |> Map.add WarehouseA [WarehouseA]
    |> Map.add WarehouseB [WarehouseB]
let stateAfter5Hours = { world with
    StockedCargos = stockedCargosAfterUnloadingInWarehouse
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (Factory, None, 5); Boat InTransitTo (Port, None, 4) ]
}

startingState |> spend1Hour 0 = stateAfter1Hour
stateAfter1Hour |> spend1Hour 1 = stateAfter2Hours
stateAfter2Hours |> spend1Hour 2 = stateAfter3Hours
stateAfter3Hours |> spend1Hour 3 = stateAfter4Hours
stateAfter4Hours |> spend1Hour 4 = stateAfter5Hours

#load "Infra.fs"
open TransportTycoon.Infra

let finalState = 
    computeHowLongItTakesToDeliver [ WarehouseA; WarehouseB ]
        [ Truck WaitingAt Factory; Truck WaitingAt Factory; Boat WaitingAt Port ]
(snd finalState).History |> writeLogs
