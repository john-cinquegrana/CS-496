(* ******************************* *)
(* Homework Assignment number 2 *)
(* @author: John Cinquegrana *)
(* @pledge: I pledge my honor that I have abided by the Stevens Honor System. *)
(* 11 February 2020 *)
(* ******************************* *)

(* A generic typing version of dtree *)
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

(*Testing graph for bf_to_dtree *)
let graph = (['x';'y';'z'],
            [([0;0;0], 0);
            ([0;0;1], 1);
            ([0;1;0], 1);
            ([0;1;1], 0);
            ([1;0;0], 1);
            ([1;0;1], 0);
            ([1;1;0], 0);
            ([1;1;1], 1)] )

let graph_2 = (['x';'y';'z'],
            [([0;0;0], 9);
            ([0;0;1], 8);
            ([0;1;0], 7);
            ([0;1;1], 6);
            ([1;0;0], 5);
            ([1;0;1], 4);
            ([1;1;0], 3);
            ([1;1;1], 2)] )

(*Computes the height of a tree with a leaf owning 0 for height *)
let rec dTree_height = function
    | Leaf(x) -> 0
    | Node(x, t1, t2) -> 1 + max (dTree_height t1) (dTree_height t2)

(*Computes the size of a tree. The size is the number of nodes summed with the number of leaves. *)
let rec dTree_size = function
    | Leaf(x) -> 1
    | Node(x, t1, t2) -> 1 + dTree_size t1 + dTree_size t2

(*Computes and returns a list of every single valid path that can be taken in the tree.
A 0 represents a left. A 1 represent a right. *)
let rec dTree_paths = function
    | Leaf(x) -> [[]]
    | Node( x, t1, t2) -> List.map (fun ls -> 0::ls) (dTree_paths t1) @ List.map (fun ls -> 1::ls) (dTree_paths t2)

(*Returns a boolean stating if a given dtree is perfect. A dTree is perfect if all leaves are at the same height.*)
let rec dTree_is_perfect = function
    | Leaf(x) -> true
    | Node( x, t1, t2) -> dTree_is_perfect t1 && dTree_is_perfect t2 &&
                            (dTree_height t1 = dTree_height t2)

(*Maps a two functions onto each value stored in the dTree *)
let rec dTree_map (f: ('a -> 'a)) (g: ('b -> 'b)) (t: ('a, 'b) dtree) =
    match t with
    | Leaf(x) -> Leaf( g x )
    | Node( x, t1, t2) -> Node( f x, dTree_map f g t1, dTree_map f g t2)

(*Transforms a character list into a character dtree with zeroed out leaves *)
let rec list_to_tree (ls: char list) : (char, int) dtree =
    match ls with
    | [] -> Leaf(0)
    | h::t -> Node( h, list_to_tree t, list_to_tree t)


let rec replace_leaf_helper (t: ('a, 'b) dtree) (ls: (int list * int) list ) (acc: int list) : ('a, int) dtree =
    match t with
    | Leaf(x) -> Leaf( List.assoc acc ls  )
    | Node(x, t1, t2) -> Node( x, replace_leaf_helper t1 ls (acc @ [0]), replace_leaf_helper t2 ls (acc @ [1]) )

let replace_leaf_at (t: ('a, 'b) dtree) ((dis,ls): char list * (int list * int) list ) : ('a, int) dtree =
    replace_leaf_helper t ls []

let bf_to_dTree ((dis,ls): char list * (int list * int) list ) : (char, int) dtree =
    let tr = list_to_tree dis in
    replace_leaf_at tr (dis,ls)