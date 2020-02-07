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

let rec segment (cx,cy) (nx,ny) =
  if ( (cx, cy) = (nx, ny) ) then []
  else
    let newx = cx + compare cx nx in
    let newy = cy + compare cy ny in
    (newx, newy) :: segment (newx, newy) (nx, ny)

let rec coverage ((start::p):coded_pic):coord list =
  segment start (List.h p) :: coverage p

let coverage_f ((start::p):coded_pic):coord list =
  List.foldr_right segment p start

let equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool =
  let subset list1 list2 =
    match list1 with
    | [] -> true
    | h::t -> (List.mem h list2) && (subset t list2)
  in
  subset list1 list2 && subset list2 list1

let height (p:coded_pic):int =
  let rec maxy max = function
    | (x,y)::t -> if ( y > max ) then y else max
    | [] -> max
  in
  let rec miny min = function
    | (x,y)::t -> if ( y < min ) then y else min
    | [] -> min
  in
  (maxy 0 p) - (miny 0 p)

let width (p:coded_pic):int =
  let rec maxx max = function
    | (x,y)::t -> if ( x > max ) then x else max
    | [] -> max
  in
  let rec minx min = function
    | (x,y)::t -> if ( x < min ) then x else min
    | [] -> min
  in
  (maxx 0 p) - (minx 0 p)

let tile ((dx,dy):coord) (p:coded_pic) : coded_pic list list =
  let iter = 1
  let rec list_shift (sx,sy) list =
    match list with
    | (x,y)::t -> (x+sx, y+sy) :: list_shift (sx,sy) t
    | [] -> []
  in
  let rec hor_tile (shift:int) (scale:int) (limit:int) (org:coded_pic) (re:coded_pic): coded_pic list =
    match shift with
    | 


let tri_aligned ((x1,y1):coord) ((x2,y2):coord) ((x3,y3):coord):bool =
  failwith "Implement"

let rec compress (p:coded_pic):coded_pic =
  failwith "Implement"