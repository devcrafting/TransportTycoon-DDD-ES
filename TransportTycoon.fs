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
type Hours = int
type Leg = Leg of from:Location * to':Location * Hours
type Path = Path of Leg list

type Event =
    | Departed of destination:Location * EventData
    | Arrived of EventData
and EventData = {
    Time: Hours
    //TransportId: int
    Kind: Transport
    Location: Location
    Cargo: Cargo
}
and Cargo = {
    //CargoId: int
    Destination: Location
    //Origin: Location
}

type World = {
    StockedCargos: Map<Location, Location list>
    Transports: (Transport * Position) list
    History: Event list
}

let Truck position positionData = Truck, position positionData
let Boat position positionData = Boat, position positionData
let Path legs = 
    let (Leg (_, destination, _)) = legs |> List.last
    destination, Path legs 

let leg fromLocation toLocation hours = Leg (fromLocation, toLocation, hours)
let paths = Map.ofList [
    Path [leg Factory Port 1; leg Port WarehouseA 4]
    Path [leg Factory WarehouseB 5]
]

let private pathTo destination fromLocation =
    let (Path path) = paths |> Map.find destination
    let (Leg (_, to', hours)) = path |> List.find (function Leg (from, _, _) -> from = fromLocation)
    to', Some destination, hours

let private goBackFrom destination fromLocation =
    let (Path path) = paths |> Map.find destination
    let (Leg (from, _, hours)) = path |> List.find (function Leg (_, to', _) -> to' = fromLocation)
    from, None, hours

let private tryLoading transport fromRemainingStockedCargos position currentTime events =
    match position with
    | WaitingAt location ->
        match fromRemainingStockedCargos |> Map.tryFind location with
        | Some (firstCargoDestination::remainingCargos) ->
            let remainingCargos =
                fromRemainingStockedCargos |> Map.remove location
                |> Map.add location remainingCargos
            let nextLocation, cargo, remainingHours = pathTo firstCargoDestination location
            remainingCargos,InTransitTo (nextLocation, cargo, remainingHours),
                events @ [Departed (
                            nextLocation,
                            { Time = currentTime
                              Kind = transport
                              Location = location
                              Cargo = { Destination = firstCargoDestination } })]
        | _ -> fromRemainingStockedCargos, position, events
    | _ -> fromRemainingStockedCargos, position, events

let load currentTime world =
    let remainingStockedCargos, transports, events =
        world.Transports
        |> List.fold (fun (remainingStockedCargos, transports, events) (transport, position) ->
            let remainingStockedCargos, newPosition, events =
                tryLoading transport remainingStockedCargos position currentTime events
            remainingStockedCargos,
                (transport, newPosition) :: transports,
                events) (world.StockedCargos, [], [])
    { world with
        StockedCargos = remainingStockedCargos
        Transports = transports |> List.rev
        History = world.History @ events }

let move world =
    let transports =
        world.Transports
        |> List.map (fun (transport, position) ->
            transport,
            match position with
            | InTransitTo (nextLocation, Some destination, 1) ->
                UnloadingAt (nextLocation, destination)
            | InTransitTo (nextLocation, None, 1) ->
                WaitingAt nextLocation
            | InTransitTo (nextLocation, destination, hours) ->
                InTransitTo (nextLocation, destination, hours - 1)
            | _ -> position)
    { world with Transports = transports}            

let unload world =
    let stockedCargos, transports =
        world.Transports
        |> List.fold (fun (stockedCargos, transports) (transport, position) ->
            let stockedCargos, newPosition =
                match position with
                | UnloadingAt (location, destination) ->
                    let newStock = destination :: (stockedCargos |> Map.tryFind location |> Option.defaultValue [] |> List.rev)
                    stockedCargos |> Map.add location newStock,
                    goBackFrom destination location |> InTransitTo
                | _ -> stockedCargos, position                
            stockedCargos, (transport, newPosition)::transports) (world.StockedCargos, [])
    { world with Transports = transports |> List.rev; StockedCargos = stockedCargos }            

let spend1Hour currentTime = (load currentTime) >> move >> unload

let rec private runUntilFulfilling (request: Location list) currentTime world =
    if world.StockedCargos |> Map.filter (fun _ stock -> stock |> List.isEmpty |> not) |> Map.toList = (request |> List.groupBy id) then
        currentTime, world
    else
        let nextWorld = world |> spend1Hour currentTime
        runUntilFulfilling request (currentTime + 1) nextWorld

let computeHowLongItTakesToDeliver cargosRequest withTransports =
    { Transports = withTransports
      StockedCargos = Map.ofList [(Factory, cargosRequest)]
      History = [] }
    |> runUntilFulfilling cargosRequest 0
