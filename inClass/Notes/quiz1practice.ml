(* ******************************* *)
(* Practice for the first quiz of the semester *)
(* 5 February 2020 *)
(* ******************************* *)

type dir = North | East | South | West
type snake = dir list
type event = Apple | Move of dir
type run = event list

let rec dropLast = function
    | [x] -> []
    | [] -> failwith "not possible"
    | h::t -> h :: dropLast t


let move s = List.hd s :: dropLast s

let eat_apple s = List.hd s :: s

let conflicting d1 d2 =
    match d1,d2 with
    | North,South -> true
    | South,North -> true
    | East, West -> true
    | West, East -> true
    | _ -> false

let change_dir s newdir =
    if not(conflicting newdir (List.hd s) ) then newdir :: dropLast s
    else move s

let has_duplicates = function
    | [] -> false
    | h::t -> not( h in t)


let rec coverage ((x,y):int * int)  (s:snake) : (int*int) list =
    match s with
    | North::t -> (x,y+1):: coverage (x,y+1) t
    | East::t -> (x+1, y):: coverage (x+1,y) t
    | West::t -> (x-1, y):: coverage (x-1,y) t
    | South::t -> (x, y-1):: coverage (x,y-1) t
    | [] -> []

let bites_tail s =
    has_duplicates (coverage (0,0) s)

let s:snake = [North;North;West;West];;