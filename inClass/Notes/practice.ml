(* ******************************* *)
(* Well-known higher-order function schemes *)
(* 27 Jan 2020 *)
(* ******************************* *)


(* Map *)

let add1 i = i+1
let upperc c = Char.uppercase_ascii c
let is_zero i = i=0
                
let rec succl (l:int list):int list =
  match l with
  | [] -> []
  | h::t -> add1 h :: succl t
              
let rec upperl (l:char list):char list =
  match l with
  | [] -> []
  | h::t -> upperc h :: upperl t

let rec check_zero (l:int list):bool list =
  match l with
  | [] -> []
  | h::t -> is_zero h :: check_zero t

let rec map: ('a -> 'b)  -> 'a list -> 'b list = fun f l ->
  match l with
  | [] -> []
  | h::t -> f h ::  map f t

let succl' = map add1
let upperl' = map upperc
let check_zero' = map is_zero

(* Filter *)
    
let is_pos i = i>0
let is_upper c = Char.uppercase_ascii c=c
let is_not_empty l = l!=[]
                   
let rec greater_than_zero (l:int list):int list =
  match l with
  | [] -> []
  | h::t ->
    if is_pos h
    then h::greater_than_zero t
    else greater_than_zero t

let rec uppercase (l:char list):char list =
  match l with
  | [] -> []
  | h::t ->
    if is_upper h
    then h::uppercase t
    else uppercase t

let rec non_empty (l:'a list list):'a list list =
  match l with
  | [] -> []
  | h::t ->
    if is_not_empty h
    then h:: non_empty t
    else non_empty t


let rec filter: ('a -> bool) -> 'a list -> 'a list = fun p l ->
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h::filter p t
    else filter p t

let greater_than_zero' = filter is_pos    
let uppercase' = filter is_upper
let non_empty' = filter is_not_empty



let rec suml (l:int list):int =
  match l with
  | [] -> 0
  | h::t -> h + suml t

let rec andl (l:bool list):bool =
  match l with
  | [] -> true
  | h::t -> h && andl t

let rec concat (l:'a list list):'a list =
  match l with
  | [] -> []
  | h::t -> h @ concat t
          

let rec foldr: ('a -> 'b  -> 'b)  -> 'b -> 'a list -> 'b = fun f a l ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let suml' = foldr (fun i j -> i+j) 0
let andl' = foldr (fun i j -> i&&j) true
let concat' = foldr (fun i j -> i @ j) []

(* ******************************* *)
(* Review of past class and ADTs   *)
(* 29 Jan 2020 *)
(* ******************************* *)

let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t 

let rec a l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: a t l2

(* Algebraic Data Types *)

type prim_color = Red | Green | Blue

let next:prim_color -> prim_color = fun c ->
  match c with
  | Red -> Green
  | Green -> Blue
  | Blue -> Red

type btreei = Empty | Node of int*btreei*btreei

let t1 = Node(40,
              Node(20,Empty,Empty),
              Node(70,
                   Node(44,Empty,Empty),
                   Empty))

let rec sizet:btreei -> int = function
  | Empty -> 0
  | Node(i,lt,rt) -> 1 + sizet lt + sizet rt

let rec sumt:btreei -> int = function
  | Empty -> 0
  | Node(i,lt,rt) -> i+sumt lt + sumt rt

let rec bump:btreei -> btreei = function
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(i+1,bump lt, bump rt)

let rec m:btreei -> btreei = function
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(i,m rt, m lt)
                   
let rec maxt = function
  | Node(i,Empty,Empty) -> i
  | Node(i,Empty,rt) -> max i (maxt rt)
  | Node(i,lt,Empty) -> max i (maxt lt)
  | Node(i,lt,rt) -> max i (max (maxt lt) (maxt rt))
  | _ -> failwith "Cannot take max of empty tree"

let rec mint = function
  | Node(i,Empty,Empty) -> i
  | Node(i,Empty,rt) -> min i (mint rt)
  | Node(i,lt,Empty) -> min i (mint lt)
  | Node(i,lt,rt) -> min i (min (mint lt) (mint rt))
  | _ -> failwith "Cannot take min of empty tree"


let rec is_bst = function
  | Empty -> true
  | Node(i,Empty,Empty) -> true
  | Node(i,(Node(j,ltj,rtj) as lt),Empty) -> maxt lt < i && is_bst lt
  | Node(i,Empty,(Node(j,ltj,rtj) as rt)) -> j<mint rt && is_bst rt
  | Node(i,lt,rt) -> maxt lt <i && i< mint rt && is_bst lt && is_bst rt
                    

type 'a btree = Empty
              | Node of 'a * 'a btree * 'a btree
              

let t1 = Node(40,
              Node(20,Empty,Empty),
              Node(70,
                   Node(44,Empty,Empty),
                   Empty))

let rec mapt: ('a ->'b) -> 'a btree -> 'b btree = fun f t ->
  match t with
  | Empty -> Empty
  | Node(i,lt,rt) -> Node(f i,mapt f lt, mapt f rt)

let rec foldt: 'b  -> ('a -> 'b  -> 'b -> 'b) -> 'a btree -> 'b = fun a f t ->
  match t with
  | Empty -> a
  | Node(i,lt,rt) -> f i (foldt a f lt) (foldt a f rt)

type 'a result = Ok of 'a | Error of string

let rec lookup: 'a -> ('a*'b) list -> 'b result = fun k d ->
  match d with
  | [] -> Error "not found"
  | (k',v)::t when k=k' -> Ok v
  | (k',v)::t  -> lookup k t

let rec height: 'a tree -> int = fun t ->
  match t with
  | Node(i, [] ) -> 1
  | Node( i, ts) ->
    1 + List.fold_left (fun n r -> max n r) 0 (List.map height ts)