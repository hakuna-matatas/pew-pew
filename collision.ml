open Type
open Settings

module H = Hashtbl

type cell = int * int

type t = {
  grid         : (cell, entity list) H.t;
  prev         : (id, cell list) H.t;
  mutable coll : (entity * entity) list
}

(* Extracts unique IDs for each entity. *)
let to_id = function
| Ammo   (id, _, _) -> id
| Bullet (id, _, _) -> id
| Gun    (id, _, _) -> id
| Player (id, _, _) -> id
| Rock   (id, _, _) -> id

(* Extracts radius of each entity. *)
let to_rad = function
| Ammo   (_, r, _)
| Bullet (_, r, _)
| Gun    (_, r, _)
| Player (_, r, _)
| Rock   (_, r, _) -> r

(* Extracts position of each entity. *)
let to_pos = function
| Ammo   (_, _, pos)
| Bullet (_, _, pos)
| Gun    (_, _, pos)
| Player (_, _, pos)
| Rock   (_, _, pos) -> pos

(* Determines if object should be checked for collision. Only bullets and players
 * move per step, so we should only check those. *)
let dynamic = function
| Ammo   _ | Rock   _ | Gun _ -> false
| Bullet _ | Player _         -> true

(* Determines how many bins positions can be hashed to. Ideally want more bins, but
 * we require that no object can cover an entire bin by itself.
 * *)
let bins = 
  let r_max = max player_radius rock_radius |> max ammo_radius |> max gun_radius in
  (map_width /. r_max /. 2.1,  map_height /. r_max /. 2.1)

(* Hashes a float position to an int cell. *)
let to_cell (x, y) = 
  let x_bins, y_bins = bins in 
  (int_of_float (x /. x_bins), int_of_float (y /. y_bins))

(* Compares two cells for equality. *)
let scale = int_of_float (map_width *. map_height)
let cell_compare (x1, y1) (x2, y2) = (x1 * scale + y1) - (x2 * scale + y2)

(* Calculates all possible cells that entity [e] can be hashed to. 
 *
 * caveat: only checks 4/8 extremal points (i.e. the diagonals). 
 * Can fail on edge cases, but these are rare enough to make the tradeoff for speed.
 * *)
let to_cells e = 
  let x, y = to_pos e in 
  let r = (to_rad e) *. (sqrt 2.0) /. 2.0 in
  let l = List.map to_cell [(x-.r,y-.r); (x+.r,y-.r); (x-.r,y+.r); (x+.r,y+.r)] in
  List.sort_uniq cell_compare l

(* Removes entity [e] from bin [b]. *)
let bin_remove b e =
  let rec bin_remove' acc = function
  | []     -> acc
  | h :: t -> 
    if to_id h = to_id e then bin_remove' acc t
    else bin_remove' (h :: acc) t in
  bin_remove' [] b

(* Removes and then adds entity [e] from bin [b].
 *
 * Does not preserve order of bin [b]. *)
let bin_replace b e = e :: (bin_remove b e)

(* Finds the squared distance between two points. *)
let sqdist (x1, y1) (x2, y2) =
  let x = x2 -. x1 in
  let y = y2 -. y1 in
    (x*.x) +. (y*.y)

let intersect e1 e2 =
  let r = (to_rad e1) +. (to_rad e2) in
  sqdist (to_pos e1) (to_pos e2) <= (r*.r)

let create () = {grid = H.create 16; prev = H.create 16; coll = []}

(* Checks for collisions between entity [e] and
 * all entities in [bin], and returns a list of all new collisions.
 *
 * Precondition: entity [e] is not in [bin]. *)
let check e bin = 
  let rec check' acc = function
  | []      -> acc
  | e' :: t -> if intersect e e' 
    then check' ((e, e') :: acc) t
    else check' acc t in
  check' [] bin

(* Inserts entity [e] associated with [cells] into the spatial hashmap. *)
let rec insert g e cells = match cells with
| []     -> ()
| c :: t -> 
  let _ = if H.mem g.grid c then
    let b  = H.find g.grid c in
    let _  = if dynamic e then g.coll <- ((check e b) @ g.coll) else () in
    let b' = bin_replace b e in
    H.replace g.grid c b'
  else
    H.add g.grid c [e] in
  insert g e t

(* Adds entity [e] into the collision detection data structure. Keeps track
 * of its position and all new collisions introduced by this object.
 *
 * requires: [add g e] is only called after [remove g e], or from [update g e].
 * Assumes that entity [e] is not in the data structure.
 * *)
let rec add g e =
  let id = to_id e in
  let cells = to_cells e in
  let _ = H.replace g.prev id cells in
  insert g e cells

(* Removes all collisions involving entity [e] from the
 * collision list. 
 *
 * Does not preserve order of list.
 * *)
let uncheck g e = 
  let rec uncheck' g e acc = function
  | []           -> acc
  | (e1, e2) :: t -> 
    let id = to_id e in
    if (id = to_id e1) || (id = to_id e2) then uncheck' g e acc t
    else uncheck' g e ((e1, e2) :: t) t in
  g.coll <- uncheck' g e [] g.coll

(* Deletes entity [e] associated with [cells] from the spatial hashmap. *)
let rec delete g e cells = match cells with
| []     -> ()
| c :: t ->
  let _ = if H.mem g.grid c then
    let b  = H.find g.grid c in
    let _ = uncheck g e in
    let b' = bin_remove b e in
    if b' = [] then H.remove g.grid c
    else H.replace g.grid c b'
  else () in delete g e t

let remove g e = 
  let id = to_id e in
  if not (H.mem g.prev id) then () else
  let cells  = H.find g.prev id in
  delete g e cells

let test g e = e
  |> to_cells
  |> List.map (fun c -> Hashtbl.find_opt g.grid c)
  |> List.filter (fun b -> match b with None -> false | _ -> true)
  |> List.map (fun b -> match b with Some b' -> b' | _ -> failwith "Unreachable")
  |> List.map (fun b -> check e b)
  |> List.flatten

let update g e = remove g e; add g e

let free g (w, h) =
  let rec free' () = 
    let pos = (Random.float w, Random.float h) in
    if H.mem g.grid (to_cell pos) then free' () else pos in
  free' ()

let all g = g.coll
