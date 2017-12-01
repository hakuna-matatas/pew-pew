
(******************************************************************************
   These Client objects are the implemntations for the data types that will be
   parsed for the client. These are the objects that the json objects will be
   parsed into and the parsed out of.
 ******************************************************************************)

(******************************************************************************
   TYPES
 ******************************************************************************)

(* The [lobby_id] is implemented by an integer*)
type lobby_id = int

(* The [id] is the id of a player and is implemented by a unique string. *)
type id = string

(* The [location] is implemented using an int tuple that holds the x coordinate
   in the first element of the tuple and the y coordinate in the other element.*)
type location = int * int

(* The [direction] is implemented by a variant *)
type direction =
  | North
  | NW
  | West
  | SW
  | South
  | SE
  | East
  | NE

(* The [gun_type] are the types of guns available in this world *)
type gun_type =
  | Pistol
  | Lazer

(* The [player] holds the information to display a player in the world.
   It usesthe name to know which this is, the gun to know the type of gun that
   the player is holding, the location of the player and the
   direction that the player is facing. *)
type player = {
  name: id;
  gun: gun_type;
  loc: location;
  dir: direction;
}

(* The [gun] holds the information to display a gun in the world. It uses
   the gun_type to know what the type of the gun, ammo to know how much
   ammo the gun has and the location of the gun. *)
type gun = {
  gun: gun_type;
  ammo: int;
  loc: location;
}

(* The [bullet] holds the information to display a bullet in the world.
   It usesthe gun_type to know what the type of the bullet,
   and the location of the bullet. *)
type bullet = {
  bullet: gun_type;
  loc: location;
}

(* The [state] holds the information to display the entire world. It
    contains a player list, a gun list and a bullet list to be able
    to display the entire world *)
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

(******************************************************************************
   Helper Functions
 ******************************************************************************)

(* Handles serializing data into JSON format so it is easy to convert
   maintain.
*)

open Yojson

let string_of_dir = function
  | North -> "north"
  | NW -> "northwest"
  | West -> "west"
  | SW -> "southwest"
  | South -> "south"
  | SE -> "southeast"
  | East -> "east"
  | NE -> "northeast"


let json_of_dir dir =
  let str_dir =  string_of_dir dir in `String (str_dir)

let json_of_fire () = `Null

let json_of_take () = `Null

let json_of_join lobby_id = `Int (lobby_id)

let json_of_create () = `Null

let loc_of_json json = failwith "Unimplemented"

let dir_of_json json = failwith "Unimplemented"

let guntype_of_json json = failwith "Unimplemented"

let player_of_json json = failwith "Unimplemented"

let players_of_json json = failwith "Unimplemented"

let gun_of_json json = failwith "Unimplemented"

let guns_of_json json = failwith "Unimplemented"

let bullet_of_json json = failwith "Unimplemented"

let bullets_of_json json = failwith "Unimplemented"

let state_of_json json = failwith "Unimplemented"

let lobby_of_json json = failwith "Unimplemented"

let lobbies_of_json json = failwith "Unimplemented"
