type Location =
    | Factory
    | Port
    | WarehouseA
    | WarehouseB
type Position =
    | WaitingAt of Location
    | InTransitTo of cargoDestination:Location option * Location * remainingHours:int
    | UnloadingAt of cargoDestination:Location * Location
type Transport =
    | Truck
    | Boat
type State = {
    StockedCargos: Map<Location, Location list>
    Transports: (Transport * Position) list
}

let Truck position positionData = Truck, position positionData
let Boat position = Boat, position

let initialCargos = Map.empty |> Map.add Factory [WarehouseA; WarehouseB(*; WarehouseA*)]
let startingState = {
    StockedCargos = initialCargos
    Transports = [ Truck WaitingAt Factory; Truck WaitingAt Factory; Boat (WaitingAt Port) ]
}
let availableCargos = initialCargos |> Map.find Factory
let stockedCargosAfterUnloading =
    initialCargos |> Map.remove Factory
    //|> Map.add Factory (availableCargos |> List.skip 2)
    |> Map.add Port [WarehouseA]
let stateAfter1Hour = {
    StockedCargos = stockedCargosAfterUnloading 
    Transports = [ Truck InTransitTo (None, Factory, 1); Truck InTransitTo (Some WarehouseB, WarehouseB, 4); Boat (WaitingAt Port) ]
}
let stockedCargosAfterLoadingBoat = stockedCargosAfterUnloading |> Map.remove Port
let stateAfter2Hours = {
    StockedCargos = stockedCargosAfterLoadingBoat
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (Some WarehouseB, WarehouseB, 3); Boat (InTransitTo (Some WarehouseA, WarehouseA, 3)) ]
}
let stockedCargosAfterLoadingLastCargo = stockedCargosAfterLoadingBoat |> Map.remove Factory
let stateAfter3Hours = {
    StockedCargos = stockedCargosAfterLoadingLastCargo
    Transports = [ Truck WaitingAt Factory (*UnloadingAt (WarehouseA, Port)*); Truck InTransitTo (Some WarehouseB, WarehouseB, 2); Boat (InTransitTo (Some WarehouseA, WarehouseA, 2)) ]
}
let stockedCargosAfterBufferInPort = stockedCargosAfterLoadingLastCargo// |> Map.add Port [WarehouseA]
let stateAfter4Hours = {
    StockedCargos = stockedCargosAfterBufferInPort
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (Some WarehouseB, WarehouseB, 1); Boat (InTransitTo (Some WarehouseA, WarehouseA, 1)) ]
}
let stockedCargosAfterUnloadingInWarehouse =
    stockedCargosAfterBufferInPort
    |> Map.add WarehouseA [WarehouseA]
    |> Map.add WarehouseB [WarehouseB]
let stateAfter5Hours = {
    StockedCargos = stockedCargosAfterUnloadingInWarehouse
    Transports = [ Truck WaitingAt Factory; Truck InTransitTo (None, Factory, 5); Boat (InTransitTo (None, Port, 4)) ]
}
