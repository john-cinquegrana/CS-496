(* ******************************* *)
(* Homework Assignment number 2 *)
(* @author: John Cinquegrana *)
(* @pledge: I pledge my honor that I have abided by the Stevens Honor System. *)
(* 11 February 2020 *)
(* ******************************* *)

type ('a, 'b) dtree = Node of 'a * ('a, 'b) dtree * ('a,'b ) dtree  | Leaf of 'b

(*Testing Trees*)
let tLeft:((char, int) dtree) = Node('w',
                    Node( 'x', Leaf(2), Leaf(5) ),
                    Leaf(8)
                )

let tRight:((char, int) dtree) = Node('w',
                    Node( 'x', Leaf(2), Leaf(5) ),
                    Node( 'y', Leaf(7), Leaf(5) )
                )

let graph = (['x';'y';'z'],
            [([0;0;0], 0);
            ([0;0;1], 1);
            ([0;1;0], 1);
            ([0;1;1], 0);
            ([1;0;0], 1);
            ([1;0;1], 0);
            ([1;1;0], 0);
            ([1;1;1], 1)] )

let rec dTree_height = function
    | Leaf(x) -> 0
    | Node(x, t1, t2) -> 1 + max (dTree_height t1) (dTree_height t2)

let rec dTree_size = function
    | Leaf(x) -> 1
    | Node(x, t1, t2) -> 1 + dTree_size t1 + dTree_size t2

let rec dTree_paths = function
    | Leaf(x) -> [[]]
    | Node( x, t1, t2) -> List.map (fun ls -> 0::ls) (dTree_paths t1) @ List.map (fun ls -> 1::ls) (dTree_paths t2)

let rec dTree_is_perfect = function
    | Leaf(x) -> true
    | Node( x, t1, t2) -> dTree_is_perfect t1 && dTree_is_perfect t2 &&
                            (dTree_height t1 = dTree_height t2)

let rec dTree_map (f: ('a -> 'a)) (g: ('b -> 'b)) (t: ('a, 'b) dtree) =
    match t with
    | Leaf(x) -> Leaf( g x )
    | Node( x, t1, t2) -> Node( f x, dTree_map f g t1, dTree_map f g t2)

let rec list_to_tree (ls: char list) : (char, int) dtree =
    match ls with
    | [] -> Leaf(0)
    | h::t -> Node( h, list_to_tree t, list_to_tree t)

let rec replace_single_leaf (t: ('a, 'b) dtree) ((ls, vl): int list * int ) =
    match t with
    | Leaf(x) ->
        if (ls = [] ) then
            Leaf(vl)
        else
            failwith "Illegal list"
    | Node(x, t1, t2) ->
        match ls with
            | [] -> failwith "Illegal tree"
            | 0::t -> Node( x, replace_single_leaf t1 (t,vl), t2 )
            | 1::t -> Node( x, t1, replace_single_leaf t2 (t,vl) )
            | _ -> failwith "Illegal tree"

let replace_left_at (t: ('a, 'b) dtree) ((dis,ls): char list * (int list * int) list ) =
    let rec iter_tree (tr: ('a, 'b) dtree) (ls: (int list * int) list ) =
        match ls with
        | [] -> tr
        | h::tl -> iter_tree (replace_single_leaf tr h) tl
    in
    iter_tree t ls

let bf_to_dTree ((dis,ls): char list * (int list * int) list ) =
    let tr = list_to_tree dis in
    replace_left_at tr (dis,ls)