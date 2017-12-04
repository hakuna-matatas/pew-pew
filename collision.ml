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

let solid = function
| Ammo   _ | Gun _              -> false
| Bullet _ | Player _ | Rock  _ -> true

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

let create s =
  let g = Hashtbl.create 16 in
  let l = s |> State.to_list |> List.filter solid in
  let rec create' = function
  | []     -> ()
  | h :: t -> 
    let c = to_cell h in
    let _ = if Hashtbl.mem g c then
      let b  = Hashtbl.find g c in
      let b' = bin_replace b h in
      Hashtbl.replace g c b'
    else
      Hashtbl.add g c [h] in
    create' t in
  create' l

let remove g e = 
  let id = to_id e in
  if not (solid e) then () else
  if not (Hashtbl.mem g.prev id) then () else
  let c  = Hashtbl.find g.prev id |> to_cell in
  let b  = Hashtbl.find g.grid c  in
  let b' = bin_remove b e in
  let _ = Hashtbl.remove g.prev id in
  if b' = [] then Hashtbl.remove g.grid c 
  else Hashtbl.replace g.grid c b'

let update g e =
  let _ = remove g e in
  let _ = Hashtbl.add g.prev (to_id e) e in
  let c = to_cell e in
  if Hashtbl.mem g.grid c then
    let b = Hashtbl.find g.grid c in
    Hashtbl.replace g.grid c (e :: b)
  else
    Hashtbl.add g.grid c [e]
