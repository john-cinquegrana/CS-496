(* This file defines expressed values and environments *)




type 'a result = Ok of 'a | Error of string

let return = fun v -> Ok v

let error fun s -> Error s

let (>>=): 'a result -> ('a -> 'b result) -> 'b result = fun c f ->
    match c with
    | Error s -> Error s
    | Ok v -> f v