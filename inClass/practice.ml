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
(*let non_empty' = filter is_not_empty *)



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
(* let concat' = foldr (fun i j -> i @ j) [] *)


let rec a l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h::a t l2

let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t

type color = Red | Green | Blue

let next (c:color) = 
  match c with
  | Red -> Green
  | Green -> Blue
  | Blue -> Red

type btreei = Empty | Node of int*btreei*btreei

let rec sizet: btreei -> int = function
  | Empty -> 0
  | Node(i, lt, rt) -> 1 + sizet lt + sizet rt

let rec bump: btreei -> btreei = function
  | Empty -> Empty
  | Node(i, lt, rt) -> Node( i+1, bump lt, bump rt)

let rec sum: btreei -> int = function
  | Empty -> 0
  | Node( i, lt, rt ) -> i + sum lt + sum rt

let rec pre: btreei -> int list = function
  | Empty -> []
  | Node( i, lt, rt ) -> i::( pre lt @ pre rt)

let rec inord: btreei -> int list = function
  | Empty -> []
  | Node( i, lt, rt ) -> inord lt @ [i] @ inord rt

let rec post: btreei -> int list = function
  | Empty -> []
  | Node( i, lt, rt ) -> inord lt @ inord rt @ [i]

type 'a btree = Empty | Node of int*'a btree*'a btree

let t1: int btree = Node(20,
  Node( 12, Empty, Empty),
  Node( 42,
    Node( 30, Empty, Empty),
    Empty)
)

let rec mapt f t =
  match t with
  | Empty -> Empty
  | Node( i, lt, rt ) -> Node( f i, mapt f lt, mapt f rt)

let rec f t =
  match t with
  | Empty -> Empty
  | Node(i, lt, rt) -> Node( i, f rt, f lt)

let rec foldt a f t =
  match t with
  | Empty -> a
  | Node(i, lt, rt) -> f i ( foldt a f lt) (foldt a f rt)

type ('a, 'b) result - Ok of 'a | Error of 'b

let rec lookup (e: 'a) (l: ('a*'b) list): 'b result =
  match l with
  | [] -> Error "not found"
  | (k,v)::t when k=e -> Ok v
  | (k, v)::t -> lookup e t
