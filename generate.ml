open Type
type t = int ref

let player_hp = 100
let ammo_count = 10

let create () = ref 0

let ammo n pos guns =
  let _ = n := !n + 1 in
  let gun = Random.int (List.length guns) in
  let s = map_scale () in
  {
    a_id  = string_of_int !n;
    a_gun = List.nth guns gun;
    a_pos = pos;
    a_rad = s.a_scale;
    a_amt = ammo_count
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
  let s = map_scale () in
  let rad = s.p_scale +. (Random.float (s.r_scale -. s.p_scale)) in
  {
    r_id  = string_of_int !n;
    r_pos = pos;
    r_rad = rad
  }

let player n pos id =
  let s = map_scale () in
  {
    p_id = id; 
    p_hp = player_hp;
    p_pos = pos;
    p_rad = s.p_scale;
    p_dir = N;
    p_inv = []
  }  
