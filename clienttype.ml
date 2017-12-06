open Yojson.Basic.Util

type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW
type id  = string

type ammo = {
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
  p_id   : id;
  p_name : id;
  p_hp   : int;
  p_pos  : pos;
  p_rad  : rad;
  p_dir  : dir;
  p_inv  : id list;
}

type gun = {
  g_id   : id;
  g_own  : id;
  g_pos  : pos;
  g_rad  : rad;
  g_type : id;
  g_ammo : int;
}

type rock = {
  r_pos : pos;
  r_rad : rad;
}

type state = {
  size    : float * float
  radius  : float
  ammo    : ammo list
  bullets : bullet list
  players : player list
  guns    : gun list
  rock    : rock list
}

let dir_of_json j =
  match to_string j with
  | "N" -> N | "NE" -> NE | "E" -> E | "SE" -> SE
  | "S" -> S | "SW" -> SW | "W" -> W | "NW" -> NW
  | _ -> failwith "Malformed JSON direction"

let pos_of_json j field = 
  match (List.map to_float (j |> member field |> to_list)) with
  | a :: b :: [] -> (a, b)
  | _            -> failwith "Malformed JSON position"

let ammo_of_json j = {
  a_gun = j |> member "gun"    |> to_string;
  a_rad = j |> member "rad"    |> to_float;
  a_amt = j |> member "amount" |> to_int;
  a_pos = pos_of_json j "pos";
}

let bullet_of_json j = {
  b_gun = j |> member "gun" |> to_string;
  b_rad = j |> member "rad" |> to_float;
  b_pos = pos_of_json j "pos";
}

let gun_of_json j = {
  g_id   = j |> member "id"    |> to_string
  g_type = j |> member "type"  |> to_string;
  g_own  = j |> member "owner" |> to_string;
  g_rad  = j |> member "rad"   |> to_float;
  g_ammo = j |> member "ammo"  |> to_int;
  g_pos  = pos_of_json j "pos";
}

let player_of_json j = {
  p_id   = j |> member "id"   |> to_string;
  p_name = j |> member "name" |> to_string;
  p_hp   = j |> member "hp"   |> to_int;
  p_rad  = j |> member "rad"  |> to_float;
  p_pos  = pos_of_json j "pos";
  p_dir  = dir_of_json (j |> member "dir");
  p_inv  = List.map to_string (j |> member "inv" |> to_list)
}

let rock_of_json j = {
  r_pos = pos_of_json j "pos";
  r_rad = j |> member "rad" |> to_float
}

let convert j f field = List.map (fun j' -> f j') (j |> member field)

let state_of_json j = {
  size    = pos_of_json "size";
  radius  = member "rad" j |> to_float
  ammo    = convert j ammo_of_json "ammo"
  bullets = convert j bullet_of_json "bullets"
  guns    = convert j gun_of_json "guns"
  players = convert j player_of_json "players"
  rocks   = convert j rock_of_json "rocks"
}
