let rec sum (n:int):int =
  match n with
    | 0 -> 0
    | m when m>0 -> m+ sum (m-1)
;;

let succ i = i+1
let to_upper c = Char.uppercase_ascii c
let is_zero i = i=0

let rec map: ('a->'b) -> 'a list -> 'b list = fun f l ->
  match l with
  | [] -> []
  | h::t -> f h:: map f t
;;

let succl = map succ

let rec uppercase( l:char list): char list =
  match l with
  | [] -> []
  | h::t -> 
    if Char.uppercase_ascii h=h
    then h::uppercase t
    else uppercase t

let rec non_empty (l:'a list list):'a list list =
  match l with
  | [] -> []
  | h::t -> 
    if h!=[]
    then h::non_empty t 
    else non_empty t

let rec filter: ('a -> bool) -> 'a list -> 'a list = fun p l ->
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h::filter p t
    else filter p t

let rec foldr: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = fun f a l ->
  match l with
  | [] -> a
  | h::y -> f h (foldr f a t) 

let my_add i j = i + j
let my_and i j = i && j
let my_append i j = i @ j

let suml' = foldr my_add 0
let andl' = foldr my_and true
let concat' = foldr my_append ""