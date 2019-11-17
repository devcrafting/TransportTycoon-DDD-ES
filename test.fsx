#load "TransportTycoon.fs"

open TransportTycoon

let initialCargos = Map.empty |> Map.add Factory [WarehouseA; WarehouseB(*; WarehouseA*)]
let startingState = {
    StockedCargos = initialCargos
    Transports = [ Truck WaitingAt Factory; Truck WaitingAt Factory; Boat WaitingAt Port ]
}
let availableCargos = initialCargos |> Map.find Factory
let stockedCargosAfterUnloading =
    initialCargos |> Map.remove Factory
    //|> Map.add Factory (availableCargos |> List.skip 2)
    |> Map.add Port [WarehouseA]
let stateAfter1Hour = {
    StockedCargos = stockedCargosAfterUnloading 
    Transports = [ Truck InTransitTo (Factory, None, 1); Truck InTransitTo (WarehouseB, Some WarehouseB, 4); Boat WaitingAt Port ]
}
let stockedCargosAfterLoadingBoat = stockedCargosAfterUnloading |> Map.remove Port
let stateAfter2Hours = {
    StockedCargos = stockedCargosAfterLoadingBoat
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (WarehouseB, Some WarehouseB, 3); Boat InTransitTo (WarehouseA, Some WarehouseA, 3) ]
}
let stockedCargosAfterLoadingLastCargo = stockedCargosAfterLoadingBoat |> Map.remove Factory
let stateAfter3Hours = {
    StockedCargos = stockedCargosAfterLoadingLastCargo
    Transports = [ Truck WaitingAt Factory (*UnloadingAt (WarehouseA, Port)*); Truck InTransitTo (WarehouseB, Some WarehouseB, 2); Boat InTransitTo (WarehouseA, Some WarehouseA, 2) ]
}
let stockedCargosAfterBufferInPort = stockedCargosAfterLoadingLastCargo// |> Map.add Port [WarehouseA]
let stateAfter4Hours = {
    StockedCargos = stockedCargosAfterBufferInPort
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (WarehouseB, Some WarehouseB, 1); Boat InTransitTo (WarehouseA, Some WarehouseA, 1) ]
}
let stockedCargosAfterUnloadingInWarehouse =
    stockedCargosAfterBufferInPort
    |> Map.add WarehouseA [WarehouseA]
    |> Map.add WarehouseB [WarehouseB]
let stateAfter5Hours = {
    StockedCargos = stockedCargosAfterUnloadingInWarehouse
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (Factory, None, 5); Boat InTransitTo (Port, None, 4) ]
}

startingState |> spend1Hour = stateAfter1Hour
stateAfter1Hour |> spend1Hour = stateAfter2Hours
stateAfter2Hours |> spend1Hour = stateAfter3Hours
stateAfter3Hours |> spend1Hour = stateAfter4Hours
stateAfter4Hours |> spend1Hour = stateAfter5Hours

computeHowLongItTakesToDeliver [ WarehouseA ]
    [ Truck WaitingAt Factory; Truck WaitingAt Factory; Boat WaitingAt Port ]
