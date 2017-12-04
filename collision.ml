open State

let to_id = function
| Ammo   (id, _, _) -> id ^ "a"
| Bullet (id, _, _) -> id ^ "b"
| Gun    (id, _, _) -> id ^ "g"
| Player (id, _, _) -> id ^ "p"
| Rock   (id, _, _) -> id ^ "r"

let to_rad = function
| Ammo   (_, r, _)
| Bullet (_, r, _)
| Gun    (_, r, _)
| Player (_, r, _)
| Rock   (_, r, _) -> r

let to_pos = function
| Ammo   (_, _, pos)
| Bullet (_, _, pos)
| Gun    (_, _, pos)
| Player (_, _, pos)
| Rock   (_, _, pos) -> pos

let to_cell e =
  let bins = 5000.00 in
  let x, y = to_pos e in 
  let x'   = int_of_float (x /. bins) in
  let y'   = int_of_float (y /. bins) in
  (x', y')

type cell = int * int
type bin = State.entity list 
type grid = (cell, bin) Hashtbl.t
type map  = (id, State.entity) Hashtbl.t

type t = {
  grid : grid;
  prev : map;
  coll : (State.entity * State.entity) list
}

(* Does not preserve order *)
let bin_remove b e =
  let rec bin_remove' acc = function
  | []     -> acc
  | h :: t -> 
    if to_id h = to_id e then bin_remove' acc t
    else bin_remove' (h :: acc) t in
  bin_remove' [] b

(* Does not preserve order of list [l] *)
let bin_replace b e = e :: (bin_remove b e)

let add g e = 
  let c = to_cell e in
  let _ = Hashtbl.replace g.prev (to_id e) e in
  if Hashtbl.mem g.grid c then
    let b  = Hashtbl.find g.grid c in
    let b' = bin_replace b e in
    Hashtbl.replace g.grid c b'
  else
    Hashtbl.add g.grid c [e]

let create s =
  let g = {grid = Hashtbl.create 16; prev = Hashtbl.create 16; coll = []} in
  let l = s |> State.to_list in
  let rec create' = function
  | []     -> ()
  | h :: t -> add g h; create' t in
  create' l

let remove g e = 
  let id = to_id e in
  if not (Hashtbl.mem g.prev id) then () else
  let c  = Hashtbl.find g.prev id |> to_cell in
  let b  = Hashtbl.find g.grid c  in
  let b' = bin_remove b e in
  let _ = Hashtbl.remove g.prev id in
  if b' = [] then Hashtbl.remove g.grid c 
  else Hashtbl.replace g.grid c b'

let update g e = remove g e; add g e

let sqdist (x1, y1) (x2, y2) =
  let x = x2 -. x1 in
  let y = y2 -. y1 in
  (x*.x) +. (y*.y)

let collision e1 e2 =
  let r = (to_rad e1) +. (to_rad e2) in
  let rs = r*.r in
  sqdist (to_pos e1) (to_pos e2) <= rs

