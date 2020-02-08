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

let rec stretch (p:coded_pic) (factor:int) : coded_pic =
  match p with
  | (x,y)::t -> (x*factor,y*factor):: stretch t factor
  | [] -> []

let stretch_m (p:coded_pic) (factor:int) : coded_pic =
  List.map ( fun (x,y) -> (factor*x, factor*y) ) p

let rec segment ((cx,cy):coord) ((nx,ny):coord) : coord list =
  if ( (cx, cy) = (nx, ny) ) then []
  else
    let newx = cx + compare nx cx in
    let newy = cy + compare ny cy in
    (newx, newy) :: segment (newx, newy) (nx, ny)

let rec coverage ((start::p):coded_pic): coord list =
  let rec fill ((start::p):coded_pic): coord list =
    match p with
    | [] -> []
    | _ -> segment start (List.hd p) @ fill p
  in
  start :: fill (start::p)

let rec last_element = function
  | [] -> failwith "Bad call to last_element"
  | [x] -> x
  | h::t -> last_element t

let coverage_f ((start::p):coded_pic):coord list =
  List.fold_left (fun arr c -> arr @ segment (last_element arr) c) [start] p

let equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool =
  let rec subset list1 list2 =
    match list1 with
    | [] -> true
    | h::t -> (List.mem h list2) && (subset t list2)
  in
  subset cp1 cp2 && subset cp2 cp1

let rec max_y (arr:coded_pic) : int =
  List.fold_right (fun (x2,y2) num -> if num > y2 then num else y2) arr 0

let rec min_y (arr:coded_pic) : int =
  List.fold_right (fun (x2,y2) num -> if num < y2 then num else y2) arr 0

let height (p:coded_pic):int = (max_y p) - (min_y p)

let rec max_x (arr:coded_pic) : int =
  List.fold_right (fun (x2,y2) num -> if num > x2 then num else x2) arr 0

let rec min_x (arr:coded_pic) : int =
  List.fold_right (fun (x2,y2) num -> if num < x2 then num else x2) arr 0

let width (p:coded_pic):int = (max_x p) - (min_x p)

let translate (p:coded_pic) ( (x,y): int * int) : coded_pic =
  List.map (fun (left, right) -> (left+x, right+y) ) p

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

let tri_aligned (c1:coord) (c2:coord) (c3:coord):bool =
  List.mem c2 ( segment c1 c3 )

let rec compress (p:coded_pic):coded_pic =
  match p with
  | [] -> []
  | c1::c2::c3::t ->
    if tri_aligned c1 c2 c3
      then compress (c1::c3::t)
      else c1 :: compress (c2::c3::t)
  | _ -> p

let cp3 = [(0,0);(1,0);(2,0);(2,2);(0,2);(0,0)]