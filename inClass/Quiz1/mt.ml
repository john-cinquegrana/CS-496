(* ******************************* *)
(* @Authors: John Cinquegrana, Glen Farbanish *)
(* @Pledge: I pledge my honor that I have abided by the Stevens Honor System.*)
(* 7 Feb 2020 *)
(* Quiz 1 in CS 496 of the 2020s semester *)
(* ******************************* *)

type 'a tree = Empty | Node of 'a*'a tree*'a tree

let rec length = function
    | [] -> 0
    | h::t -> 1 + length t

let value tree = 
    match tree with
    | Node( v, t1, t2 ) -> v
    | Empty -> 0

let split (arr: int list) : (int list * int list) =
    let rec help arr first_half limit =
        if ( limit = 0 ) then
            (first_half, arr)
        else
            help (List.tl arr) ( first_half @ [List.hd arr] ) (limit-1)
    in
    help arr [] ( (length arr) / 2)

let rec mt (f:(int -> int)) (ls:(int list)) : (int tree) =
    match ls with
    | [] -> Empty
    | [i] -> Node( f i, Empty, Empty)
    | _ ->
        let (x, y) = (split ls) in
        let t1 = mt f x in
        let t2 = mt f y in
        Node( f( (value t1) + (value t2) ), t1, t2 )

