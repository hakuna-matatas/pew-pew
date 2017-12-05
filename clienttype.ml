open Yojson.Basic.Util

type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW
type id  = string


type ammo = {
  a_id  : id;
  a_gun : id;
  a_pos : pos;
  a_rad : rad;
  a_amt : int;
}

type bullet = {
  b_gun  : id;
  b_pos  : pos;
  b_rad  : rad;
}

type player = {
  p_id  : id;
  p_hp  : int;
  p_pos : pos;
  p_rad : rad;
  p_dir : dir;
  p_inv : id list;
}

type gun = {
  g_id   : id;
  g_cd   : int;
  g_own  : id;
  g_pos  : pos;
  g_rad  : rad;
  g_ammo : int;
}

type rock = {
  r_pos : pos;
  r_rad : rad;
}


let dir_of_json j =
  match to_string j with
  | "N" -> N | "NE" -> NE | "E" -> E | "SE" -> SE
  | "S" -> S | "SW" -> SW | "W" -> W | "NW" -> NW
  | _ -> N

let json_to_pos j= match (List.map to_float (j |> member "pos" |> to_list)) with
  | a::b::c -> (a,b)
  | _ -> (0.0,0.0)

let ammo_of_json j = {
  a_id= j |> member "id" |> to_string;
  a_gun= j |> member "gun" |> to_string;
  a_pos= json_to_pos j;
  a_rad= j |> member "rad" |> to_float;
  a_amt= j |> member "amount" |> to_int;
}


let bullet_to_json b =
  let x, y = b.b_pos in
  `Assoc [
    ("gun" , `String b.b_gun);
    ("pos" , `List   [`Float x; `Float y]);
    ("rad" , `Float  b.b_rad)
  ]

let bullet_of_json j = {
  b_gun= j |> member "gun" |> to_string;
  b_pos= json_to_pos j;
  b_rad= j |> member "rad" |> to_float;
}

let gun_to_json g =
  let x, y = g.g_pos in
  `Assoc [
    ("id"     , `String g.g_id);
    ("cd"     , `Int g.g_cd);
    ("owner"  , `String g.g_own);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad"    , `Float  g.g_rad);
    ("ammo"   , `Int    g.g_ammo)
  ]

let gun_of_json j = {
  g_id= j |> member "id" |> to_string;
  g_cd= j |> member "cd" |> to_int;
  g_own= j |> member "owner" |> to_string;
  g_pos= json_to_pos j;
  g_rad= j |> member "rad" |> to_float;
  g_ammo= j |> member "ammo" |> to_int;
}

let player_of_json j = {
  p_id= j |> member "id" |> to_string;
  p_hp= j |> member "hp" |> to_int;
  p_pos= json_to_pos j;
  p_rad= j |> member "rad" |> to_float;
  p_dir= dir_of_json (j |> member "dir");
  p_inv= List.map to_string (j |> member "inv" |> to_list)
}

let rock_of_json j = {
  r_pos= json_to_pos j;
  r_rad= j |> member "rad" |> to_float
}
