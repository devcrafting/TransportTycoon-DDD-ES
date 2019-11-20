module TransportTycoon.Domain

type Location =
    | Factory
    | Port
    | WarehouseA
    | WarehouseB
type Position =
    | WaitingAt of Location
    | InTransitTo of Location * cargoDestination:Location option * remainingHours:int
    | ArrivingIn of Location * cargoDestination:Location
type Transport =
    | Truck
    | Boat
type Hours = int
type Leg = Leg of from:Location * to':Location * Hours
type Path = Path of Leg list

type Event =
    | DepartedTo of destination:Location * EventData
    | ArrivedIn of EventData
and EventData = {
    Time: Hours
    //TransportId: int
    Kind: Transport
    Location: Location
    Cargo: Cargo option
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
    from, hours

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
                events @ [DepartedTo (
                            nextLocation,
                            { Time = currentTime
                              Kind = transport
                              Location = location
                              Cargo = Some { Destination = firstCargoDestination } })]
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

let move currentTime world =
    let transports, events =
        world.Transports
        |> List.map (fun (transport, position) ->
            match position with
            | InTransitTo (nextLocation, Some destination, 1) ->
                (transport, ArrivingIn (nextLocation, destination)),
                    ArrivedIn {
                        Time = currentTime
                        Kind = transport
                        Location = nextLocation
                        Cargo = Some { Destination = destination }
                    } |> Some
            | InTransitTo (nextLocation, None, 1) ->
                (transport,WaitingAt nextLocation),
                    ArrivedIn {
                        Time = currentTime
                        Kind = transport
                        Location = nextLocation
                        Cargo = None
                    } |> Some
            | InTransitTo (nextLocation, destination, hours) ->
                (transport, InTransitTo (nextLocation, destination, hours - 1)), None
            | _ -> (transport, position), None)
        |> List.unzip        
    { world with
        Transports = transports
        History = world.History @ (events |> List.choose id) }            

let unload currentTime world =
    let stockedCargos, transports, events =
        world.Transports
        |> List.fold (fun (stockedCargos, transports, events) (transport, position) ->
            let stockedCargos, newPosition, events =
                match position with
                | ArrivingIn (location, destination) ->
                    let newStock = destination :: (stockedCargos |> Map.tryFind location |> Option.defaultValue [] |> List.rev)
                    let nextLocation, remainingHours = goBackFrom destination location
                    stockedCargos |> Map.add location newStock,
                        InTransitTo (nextLocation, None, remainingHours),
                        events @ [ DepartedTo (
                                    nextLocation,
                                    { Time = currentTime
                                      Kind = transport
                                      Location = location
                                      Cargo = None } ) ]
                | _ -> stockedCargos, position, events                
            stockedCargos, (transport, newPosition)::transports, events) (world.StockedCargos, [], [])
    { world with
        Transports = transports |> List.rev;
        StockedCargos = stockedCargos
        History = world.History @ events }            

let spend1Hour currentTime = (load currentTime) >> (move (currentTime + 1)) >> (unload (currentTime + 1))

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
