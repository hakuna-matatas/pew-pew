include Type 
include Settings

type t = int ref

let create () = ref 0

let ammo n pos guns =
  let _ = n := !n + 1 in
  let gun = Random.int (List.length guns) in
  {
    a_id  = string_of_int !n;
    a_gun = List.nth guns gun;
    a_pos = pos;
    a_rad = ammo_radius;
    a_amt = ammo_count ()
  }

let bullet n p g = 
  let rec bullet' acc = function
  | []     -> acc
  | b :: t -> 
    let _ = n := !n + 1 in 
    bullet' ({b with b_id = string_of_int !n} :: acc) t in
  bullet' [] (g.g_fire p)

let gun n pos =
  let _ = n := !n + 1 in
  let g = Armory.create () in
  { g with 
    g_id = string_of_int !n;
    g_pos = pos }

let rock n pos =
  let _ = n := !n + 1 in
  let rad = player_radius +. (Random.float (rock_radius -. player_radius)) in
  {
    r_id  = string_of_int !n;
    r_pos = pos;
    r_rad = rad
  }

let player n pos id = {
    p_id = id; 
    p_hp = player_hp;
    p_pos = pos;
    p_rad = player_radius;
    p_dir = N;
    p_inv = []
  }  
