module TransportTycoon

type Location =
    | Factory
    | Port
    | WarehouseA
    | WarehouseB
type Position =
    | WaitingAt of Location
    | InTransitTo of Location * cargoDestination:Location option * remainingHours:int
    | UnloadingAt of Location * cargoDestination:Location
type Transport =
    | Truck
    | Boat
type World = {
    StockedCargos: Map<Location, Location list>
    Transports: (Transport * Position) list
}
type Hours = int
type Leg = Leg of from:Location * to':Location * Hours
type Path = Path of Leg list

let Truck position positionData = Truck, position positionData
let Boat position positionData = Boat, position positionData

let leg fromLocation toLocation hours = Leg (fromLocation, toLocation, hours)
let paths = Map.ofList [
    (WarehouseA, Path [leg Factory Port 1; leg Port WarehouseA 4])
]

let private pathTo destination fromLocation =
    let (Path path) = paths |> Map.find destination
    let (Leg (_, to', hours)) = path |> List.find (function Leg (from, _, _) -> from = fromLocation)
    to', Some destination, hours

let private tryLoading fromRemainingStockedCargos position =
    match position with
    | WaitingAt location ->
        match fromRemainingStockedCargos |> Map.tryFind location with
        | Some (firstCargoDestination::remainingCargos) ->
            fromRemainingStockedCargos |> Map.remove location
            |> Map.add location remainingCargos, pathTo firstCargoDestination location |> InTransitTo
        | _ -> fromRemainingStockedCargos, position
    | _ -> fromRemainingStockedCargos, position

let load world =
    let remainingStockedCargos, transports =
        world.Transports
        |> List.fold (fun (remainingStockedCargos, transports) (transport, position) ->
            let remainingStockedCargos, newPosition = tryLoading remainingStockedCargos position
            remainingStockedCargos, (transport, newPosition) :: transports) (world.StockedCargos, [])
    { world with
        StockedCargos = remainingStockedCargos
        Transports = transports |> List.rev }

let move world = world

let unload world = world

let spend1Hour : (World -> World) = load >> move >> unload

let computeHowLongItTakesToDeliver _ = 0