open Yojson.Basic.Util

type pos  = (int * int)
type rad  = int
type dir  = N | NE | E | SE | S | SW | W | NW
type id   = int
type name = string

type ammo = {
  a_type : name;
  a_pos  : pos;
  a_rad  : rad;
  a_amt  : int
}

type bullet = {
  b_type : name;
  b_pos  : pos;
  b_rad  : rad
}

type player = {
  p_id   : id;
  p_name : name;
  p_hp   : int;
  p_pos  : pos;
  p_rad  : rad;
  p_dir  : dir;
  p_inv  : id list
}

type gun = {
  g_id   : id;
  g_own  : id;
  g_pos  : pos;
  g_rad  : rad;
  g_type : name;
  g_ammo : int
}

type rock = {
  r_pos : pos;
  r_rad : rad
}

type state = {
  id      : id;
  name    : name;
  size    : int * int;
  radius  : int;
  ammo    : ammo list;
  bullets : bullet list;
  players : player list;
  guns    : gun list;
  rocks   : rock list
}

type description = {
  game_name    : name;
  game_id      : id;
  game_players : name list
}

type create_response = {
  rgame_id   : id;
  rplayer_id : id;
}

let dir_of_json j =
  match to_string j with
  | "N" -> N | "NE" -> NE | "E" -> E | "SE" -> SE
  | "S" -> S | "SW" -> SW | "W" -> W | "NW" -> NW
  | _ -> failwith "Malformed JSON direction"

let pos_of_json j field =
  match (List.map to_float (j |> member field |> to_list)) with
  | a :: b :: [] -> (int_of_float a, int_of_float b)
  | _            -> failwith "Malformed JSON position"

let ammo_of_json j = {
  a_type = j |> member "type"   |> to_string;
  a_rad  = j |> member "rad"    |> to_float |> int_of_float;
  a_amt  = j |> member "amount" |> to_int;
  a_pos  = pos_of_json j "pos";
}

let bullet_of_json j = {
  b_type = j |> member "type" |> to_string;
  b_rad  = j |> member "rad"  |> to_float |> int_of_float;
  b_pos  = pos_of_json j "pos";
}

let gun_of_json j = {
  g_id   = j |> member "id"    |> to_int;
  g_type = j |> member "type"  |> to_string;
  g_own  = j |> member "owner" |> to_int;
  g_rad  = j |> member "rad"   |> to_int;
  g_ammo = j |> member "ammo"  |> to_int;
  g_pos  = pos_of_json j "pos";
}

let player_of_json j = {
  p_id   = j |> member "id"   |> to_int;
  p_name = j |> member "name" |> to_string;
  p_hp   = j |> member "hp"   |> to_int;
  p_rad  = j |> member "rad"  |> to_float |> int_of_float;
  p_pos  = pos_of_json j "pos";
  p_dir  = dir_of_json (j |> member "dir");
  p_inv  = List.map to_int (j |> member "inv" |> to_list)
}

let rock_of_json j = {
  r_pos = pos_of_json j "pos";
  r_rad = j |> member "rad" |> to_float |> int_of_float
}

let convert j f field = List.map (fun j' -> f j') (j |> member field |> to_list)

let create_post g_name p_name =
  `Assoc [
    ("game_name"   , `String g_name);
    ("player_name" , `String p_name);
  ]

let create_response_of_json j = {
  rgame_id   = member "game_id" j |> to_int;
  rplayer_id = member "player_id" j |> to_int
}

let join_response_of_json j = member "player_id" j |> to_int

let state_of_json j = {
  id      = member "id" j  |> to_int;
  name    = member "name" j |> to_string;
  size    = pos_of_json j "size";
  radius  = member "rad" j |> to_float |> int_of_float;
  ammo    = convert j ammo_of_json "ammo";
  bullets = convert j bullet_of_json "bullets";
  guns    = convert j gun_of_json "guns";
  players = convert j player_of_json "players";
  rocks   = convert j rock_of_json "rocks"
}

let description_of_json j = to_list j
  |> List.map (fun j' -> {
    game_name    = member "game_name" j' |> to_string;
    game_id      = member "game_id"   j' |> to_int;
    game_players = member "game_players" j' |> to_list |> List.map to_string
  })
