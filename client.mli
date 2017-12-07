(* Client representations of server-side games and entities.
 *
 * Contains helper functions for communicating via JSON.
 *)

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
  id    : id;
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

(* Converts the given JSON object to a state, according to the API. *)
val state_of_json : Yojson.Basic.json -> state

(* Converts the given JSON object to a description list, according to the API. *)
val description_of_json : Yojson.Basic.json -> description list
