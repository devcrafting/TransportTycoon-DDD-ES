module TransportTycoon

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
type World = {
    StockedCargos: Map<Location, Location list>
    Transports: (Transport * Position) list
}

let Truck position positionData = Truck, position positionData
let Boat position = Boat, position

let spend1Hour world = world

let computeHowLongItTakesToDeliver _ = 0