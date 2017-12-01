
(******************************************************************************
   These Client objects represent the data types that will be parsed for the
   client. These are the objects that the json objects will be parsed into
   and the parsed out of.
 ******************************************************************************)

 (* The [lobby_id] is the current lobby a player is in *)
type lobby_id = int

(* This is the [id] of a player in the world. This is unique for every player. *)
type id = string

(* The location of an entity in the game map *)
type location = int * int

(* This will be a variant with 8 directions (N, NW, W, SW, S, SE, E, NE *)
type direction =
  | North
  | NW
  | West
  | SW
  | South
  | SE
  | East
  | NE

(* The is a variant of the different types of guns *)
type gun_type =
  | Pistol
  | Lazer

(* The record holds the information for a gun inside the client *)
type gun = {
  gun: gun_type;
  ammo: int;
  loc: location;
}

(* This record holds the information for a bullet inside the client *)
type bullet = {
  bullet: gun_type;
  loc: location;
}

(* This record holds the information for a player inside the client*)
type player = {
  name: id;
  gun: gun_type;
  loc: location;
  dir: direction;
}

(* This type respresents what the http [response] can give back.*)
type state = {
  players: player list;
  guns: gun list;
  bullets: bullet list;
}

(* This type represents one lobby *)
type lobby = {
  lobby_id: lobby_id;
  players: id list;
}

(* This type represents multiple lobbies *)
type lobbies = {
  lobby_ids: lobby list;
}

val json_of_dir: direction -> Yojson.json

val json_of_fire: unit -> Yojson.json

val json_of_take: unit -> Yojson.json

val json_of_join: int -> Yojson.json

val json_of_create: unit -> Yojson.json

val loc_of_json: Yojson.json -> location

val dir_of_json: Yojson.json -> direction

val guntype_of_json: Yojson.json -> gun_type

val player_of_json: Yojson.json -> player

val players_of_json: Yojson.json -> player list

val gun_of_json: Yojson.json -> gun

val guns_of_json: Yojson.json -> gun list

val bullet_of_json: Yojson.json -> bullet

val bullets_of_json: Yojson.json -> bullet list

val state_of_json: Yojson.json -> state

val lobby_of_json: Yojson.json -> lobby

val lobbies_of_json: Yojson.json -> lobby list
