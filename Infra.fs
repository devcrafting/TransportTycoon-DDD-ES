module TransportTycoon.Infra

open TransportTycoon.Domain

let private locationToString location = location.ToString().Replace("Warehouse", "").ToUpper()

let writeLogs events =
    events
    |> List.iter (fun e ->
        let eventType, data, destinationIfAny =
            match e with
            | DepartedTo (destination, data) -> "DEPART", data, sprintf ", \"destination\": \"%s\" " (destination.ToString().ToUpper())
            | ArrivedIn data -> "ARRIVE", data, ""
        let cargoIfAny =
            match data.Cargo with
            | Some cargo -> sprintf ",\"cargo\": [{ \"cargo_id\": 0, \"destination\": \"%s\", \"origin\": \"FACTORY\"}]" (cargo.Destination |> locationToString)
            | None -> ""
        let transportId =
            match data.Kind with
            | Truck id -> id
            | Boat id -> id        
        printfn "{\"event\": \"%s\", \"time\": %i, \"transport_id\": %i, \"kind\": \"%s\", \"location\": \"%s\"%s%s}"
            eventType
            data.Time
            transportId
            (data.Kind.ToString().ToUpper())
            (data.Location |> locationToString)
            destinationIfAny
            cargoIfAny)
