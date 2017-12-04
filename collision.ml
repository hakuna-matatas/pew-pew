open State

module IDHash = struct
  type t = State.entity

  let to_id = function
  | Ammo   (id, _, _)
  | Bullet (id, _, _)
  | Gun    (id, _, _)
  | Player (id, _, _) -> id
  | Rock   (id, _, _) -> string_of_int id 

  let equal e e' = match e, e' with
  | Ammo   _ , Ammo   _ 
  | Bullet _ , Bullet _
  | Gun    _ , Gun    _ 
  | Player _ , Player _
  | Rock   _ , Rock   _ -> (to_id e = to_id e')
  | _ -> false

  let hash e = Hashtbl.hash (to_id e)
end

module PosHash = struct
  type t = State.entity
  
  let bins = 5000.00

  let to_pos = function
  | Ammo   (_, _, pos)
  | Bullet (_, _, pos)
  | Gun    (_, _, pos)
  | Player (_, _, pos)
  | Rock   (_, _, pos) -> pos
  
  let round e =
    let x, y = to_pos e in 
    let x'   = int_of_float (x /. bins) in
    let y'   = int_of_float (y /. bins) in
    (x', y')

  let hash e = Hashtbl.hash (round e)

  let equal e e' = round e = round e'
end

module Bucket = Hashtbl.Make (IDHash)
module Grid   = Hashtbl.Make (PosHash)

type t = (unit Bucket.t) Grid.t
