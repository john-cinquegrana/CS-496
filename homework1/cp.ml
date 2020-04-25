(* ******************************* *)
(* Homework Assignment number 1 *)
(* @author: John Cinquegrana *)
(* @pledge: I pledge my honor that I have abided by the Stevens Honor System. *)
(* 6 February 2020 *)
(* ******************************* *)

type coord = int*int
type coded_pic = coord list

let cp1:coded_pic = [(0,0);(2,0);(2,2);(0,2);(0,0)]

let cp2:coded_pic = [(0,0);(4,0);(4,4);(0,0)]

(* Scales a picture by a provided factor *)
let rec stretch (p:coded_pic) (factor:int) : coded_pic =
  match p with
  | (x,y)::t -> (x*factor,y*factor):: stretch t factor
  | [] -> []

(* Scales a picture by a provided factor *)
let stretch_m (p:coded_pic) (factor:int) : coded_pic =
  List.map ( fun (x,y) -> (factor*x, factor*y) ) p

(* List all the points on a path between between (cx,cy) and (nx,ny) *)
let rec segment ((cx,cy):coord) ((nx,ny):coord) : coord list =
  if ( (cx, cy) = (nx, ny) ) then []
  else
    let newx = cx + compare nx cx in
    let newy = cy + compare ny cy in
    (newx, newy) :: segment (newx, newy) (nx, ny)

(* Returns a new coded_pic with every single pixel directly between each consecutive
  pair of points included. *)
let rec coverage (ls:coded_pic): coord list =
  match ls with
  | [] -> []
  | [x] -> ls
  | start::p ->
    let rec fill (ls:coded_pic): coord list =
      match ls with
      | [] -> []
      | [x] -> []
      | elem1::elem2::p -> segment elem1 elem2 @ fill (elem2::p)
    in
    start :: fill (start::p)

(* Returns tha last element of any list *)
let rec last_element = function
  | [] -> failwith "Bad call to last_element"
  | [x] -> x
  | h::t -> last_element t

(* Returns a new coded_pic with every single pixel directly between each consecutive
  pair of points included. *)
let coverage_f (ls:coded_pic):coord list =
  match ls with
  | [] -> []
  | [x] -> ls
  | start::p ->
    List.fold_left (fun arr c -> arr @ segment (last_element arr) c) [start] p

(* Tests if two coded_pic's create an identical shape *)
let equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool =
  let rec subset list1 list2 =
    match list1 with
    | [] -> true
    | h::t -> (List.mem h list2) && (subset t list2)
  in
  let c1 = coverage cp1 in
  let c2 = coverage cp2 in
  subset c1 c2 && subset c2 c1

(* Returns the maximum y encountered in a coded_pic *)
let rec max_y (ls:coded_pic) : int =
  match ls with
  | [] -> failwith "Bad argument"
  | [(a,b)] -> b
  | (a,b)::arr ->
    List.fold_right (fun (x2,y2) num -> if num > y2 then num else y2) arr b

(* Returns the minimum y encountered in a coded_pic *)
let rec min_y (ls:coded_pic) : int =
  match ls with
  | [] -> failwith "Bad argument"
  | [(a,b)] -> b
  | (a,b)::arr ->
    List.fold_right (fun (x2,y2) num -> if num < y2 then num else y2) arr b

let height (p:coded_pic):int = (max_y p) - (min_y p)

(* Returns the maximum x encountered in a coded_pic *)
let rec max_x (ls:coded_pic) : int =
  match ls with
  | [] -> failwith "Bad argument"
  | [(a,b)] -> a
  | (a,b)::arr ->
    List.fold_right (fun (x2,y2) num -> if num > x2 then num else x2) arr a

(* Returns the minimum x encountered in a coded_pic *)
let rec min_x (ls:coded_pic) : int =
  match ls with
  | [] -> failwith "Bad argument"
  | [(a,b)] -> a
  | (a,b)::arr ->
    List.fold_right (fun (x2,y2) num -> if num < x2 then num else x2) arr a

(* Returns the overall width of a coded_pic *)
let width (p:coded_pic):int = (max_x p) - (min_x p)

(* Moves a coded pick the specified distance in the x and y directions without
  modifying the geometric shape or causing any rotation. *)
let translate (p:coded_pic) ( (x,y): int * int) : coded_pic =
  List.map (fun (left, right) -> (left+x, right+y) ) p

(* Copies a picture dx times to the right and dy times up. *)
let tile ((dx,dy):coord) (p:coded_pic) : (coded_pic list) list =
  (* Adds the sx value to each x coord and the sy value to each y coord *)
  let shift_y = height p in
  let shift_x = width p in
  let rec hor_tile (iter:int) (p:coded_pic) : coded_pic list =
    match iter with
    | 0 -> []
    | 1 -> [p]
    | x ->  
      let new_list = translate p (shift_x, 0) in
      p :: hor_tile (iter-1) new_list
  in
  let rec ver_tile (iter:int) (hor_iter:int) (p:coded_pic) : (coded_pic list) list =
    match iter with
    | 0 -> []
    | 1 -> [hor_tile hor_iter p]
    | x ->
      let new_list = translate p (0, shift_y) in
      (hor_tile hor_iter p) :: (ver_tile (iter-1) hor_iter new_list )
  in
  ver_tile dy dx p

(* Returns true of c2 is directly in between c1 and c3 *)
let tri_aligned (c1:coord) (c2:coord) (c3:coord):bool =
  List.mem c2 ( segment c1 c3 )

(* Compresses a coded_pic of a shape into its minimal representation. *)
let rec compress (p:coded_pic):coded_pic =
  match p with
  | [] -> []
  | c1::c2::c3::t ->
    if tri_aligned c1 c2 c3
      then compress (c1::c3::t)
      else c1 :: compress (c2::c3::t)
  | _ -> p