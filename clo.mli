
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
  gun_loc: location;
}

(* This record holds the information for a bullet inside the client *)
type bullet = {
  bullet: gun_type;
  bullet_loc: location;
}

(* This record holds the information for a player inside the client*)
type player = {
  name: id;
  player_gun: gun_type;
  player_loc: location;
  dir: direction;
}

(* This type respresents what the http [response] can give back.*)
type state = {
  state_players: player list;
  guns: gun list;
  bullets: bullet list;
}

(* This type represents one lobby *)
type lobby = {
  lobby_id: lobby_id;
  lobby_players: id list;
}

(* [json_of_dir] dir constructs a json of direction *)
val json_of_dir: direction -> Yojson.Basic.json

(* [json_of_fire] constructs a json body for a fire request *)
val json_of_fire: unit -> Yojson.Basic.json

(* [json_of_take] constructs a json body for a take request *)
val json_of_take: unit -> Yojson.Basic.json

(* [json_of_join] constructs a json body for a join request *)
val json_of_join: int -> Yojson.Basic.json

(* [json_of_create] constructs a json body for a create request *)
val json_of_create: unit -> Yojson.Basic.json

(* [loc_of_json] converts a json holding a location into a location record *)
val loc_of_json: Yojson.Basic.json -> location

(* [loc_of_json] converts a json holding a direciton into a direction record *)
val dir_of_json: Yojson.Basic.json -> direction

(* [guntype_of_json] converts a json holding a gun_type into a gun_type record *)
val guntype_of_json: Yojson.Basic.json -> gun_type

(* [player_of_json] converts a json holding a player into a player record *)
val player_of_json: Yojson.Basic.json -> player

(* [players_of_json] converts a json holding players into a player record list *)
val players_of_json: Yojson.Basic.json -> player list

(* [gun_of_json] converts a json holding a gun into a gun record *)
val gun_of_json: Yojson.Basic.json -> gun

(* [guns_of_json] converts a json holding players into a player record list *)
val guns_of_json: Yojson.Basic.json -> gun list

(* [bullet_of_json] converts a json holding a bullet into a bullet record *)
val bullet_of_json: Yojson.Basic.json -> bullet

(* [bullets_of_json] converts a json holding bullets into a bullet record list *)
val bullets_of_json: Yojson.Basic.json -> bullet list

(* [state_of_json] converts a json holding state into a state record *)
val state_of_json: Yojson.Basic.json -> state

(* [lobby_of_json] converts a json holding lobby into a lobby record *)
val lobby_of_json: Yojson.Basic.json -> lobby

(* [lobbies_of_json] converts a json holding mutliple lobbies into a lobby record list *)
val lobbies_of_json: Yojson.Basic.json -> lobby list
