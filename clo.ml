
(******************************************************************************
   These Client objsonects are the implemntations for the data types that will be
   parsed for the client. These are the objsonects that the jsonson objsonects will be
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
  player_gun: gun_type;
  player_loc: location;
  dir: direction;
}

(* The [gun] holds the information to display a gun in the world. It uses
   the gun_type to know what the type of the gun, ammo to know how much
   ammo the gun has and the location of the gun. *)
type gun = {
  gun: gun_type;
  ammo: int;
  gun_loc: location;
}

(* The [bullet] holds the information to display a bullet in the world.
   It usesthe gun_type to know what the type of the bullet,
   and the location of the bullet. *)
type bullet = {
  bullet: gun_type;
  bullet_loc: location;
}

(* The [state] holds the information to display the entire world. It
    contains a player list, a gun list and a bullet list to be able
    to display the entire world *)
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

(* This type represents multiple lobbies *)
type lobbies = {
  lobby_ids: lobby list;
}

(******************************************************************************
   Helper Functions
 ******************************************************************************)

(* Handles serializing data into jsonSON format so it is easy to convert
   maintain.
*)

open Yojson

(* [string_of_dir] takes a direction and gives back the string representation *)
let string_of_dir = function
  | North -> "north"
  | NW -> "northwest"
  | West -> "west"
  | SW -> "southwest"
  | South -> "south"
  | SE -> "southeast"
  | East -> "east"
  | NE -> "northeast"

(* [dir_of_string] takes a string and gives back the appropriate direction *)
let dir_of_string = function
  | "north" -> North
  | "northwest" -> NW
  | "west" -> West
  | "southwest" -> SW
  | "south" -> South
  | "southeast" -> SE
  | "east" -> East
  | "northeast" -> NE
  | _ -> failwith "Unimplemented"

(* [string_of_guntype] takes a gun and gives back its string representation *)
let string_of_guntype = function
  | Pistol -> "pistol"
  | Lazer -> "lazer"

(* [guntype_of_string] takes a string and gives back the gun that corresponds*)
let guntype_of_string = function
  | "pistol" -> Pistol
  | "lazer" -> Lazer
  | _ -> failwith "Unimplemented"

(* [json_of_dir] dir constructs a json of direction*)
let json_of_dir dir =
  let str_dir =  string_of_dir dir in `String (str_dir)

(* [json_of_fire] constructs a null body as fire does not need a body *)
let json_of_fire () = `Null

(* [json_of_take] constructs a null body as take does not need a body *)
let json_of_take () = `Null

(* [json_of_join] constructs a json body which only
   has one interger of a lobby_id in it *)
let json_of_join lobby_id = `Int (lobby_id)

(* [json_of_take] constructs a null body as take does not need a body *)
let json_of_create () = `Null

(* [loc_of_json] takes a [json] and converts it into a location. It first converts
    the json into a list and then take the two first values *)
let loc_of_json json =
  let open Yojson.Basic.Util in
  let two_vals = json |> to_list |> List.map to_int in
  let x = List.nth two_vals 0 in
  let y = List.nth two_vals 1 in
  (x,y)

(* [dir_of_json] takes [json] and builds a [direction] out of it *)
let dir_of_json json =
  let open Yojson.Basic.Util in
  json |> to_string |> dir_of_string

(* [gun_type_of_json] takes [json] and builds a [direction] out of it *)
let guntype_of_json json =
  let open Yojson.Basic.Util in
  json |> to_string |> guntype_of_string


(* [player_of_json] takes [json] and builds a [player] out of it by
    getting the name, gun, location and direction of the player and storing
    it in a record *)
let player_of_json json =
  let open Yojson.Basic.Util in
  let name = json |> member "name" |> to_string in
  let player_gun = json |> member "gun" |> guntype_of_json in
  let player_loc = json |> member "loc" |> loc_of_json in
  let dir = json |> member "dir" |> dir_of_json in
  { name = name;
    player_gun = player_gun;
    player_loc = player_loc;
    dir = dir; }


(* [players_of_json] takes a list of players and runs [player_of_json]
    on each of them *)
let players_of_json json =
  let open Yojson.Basic.Util in
  json |> member "players" |> to_list |> List.map player_of_json

(* [gun_of_json] takes [json] and builds a [gun] out of it by
    getting the gun, location and ammo of the gun and storing
    it in a record *)
let gun_of_json json =
  let open Yojson.Basic.Util in
  let gun = json |> member "gun" |> guntype_of_json in
  let ammo = json |> member "gun" |> to_int in
  let gun_loc = json |> member "loc" |> loc_of_json in
  { gun = gun;
    ammo = ammo;
    gun_loc = gun_loc; }

(* [guns_of_json] takes a list of guns and runs [gun_of_json]
    on each of them *)
let guns_of_json json =
  let open Yojson.Basic.Util in
  json |> member "guns" |> to_list |> List.map gun_of_json

(* [bullet_of_json] takes [json] and builds a [bullet] out of it by
    getting the gun and location storing it in a record *)
let bullet_of_json json =
  let open Yojson.Basic.Util in
  let gun = json |> member "gun" |> guntype_of_json in
  let loc = json |> member "loc" |> loc_of_json in
  { bullet = gun;
    bullet_loc = loc; }

(* [bullets_of_json] takes a list of bullets and runs [bullet_of_json]
    on each of them *)
let bullets_of_json json =
  let open Yojson.Basic.Util in
  json |> member "bullets" |> to_list |> List.map bullet_of_json

(* [state_of_json] takes [json] and builds a [state] out of it by
    getting the players list, guns list and bullets list and storing
    it in a state record *)
let state_of_json json =
  let open Yojson.Basic.Util in
  let players = json |> member "points" |> to_list |> List.map player_of_json in
  let guns = json |> member "guns" |> to_list |> List.map gun_of_json in
  let bullets = json |> member "bullets" |> to_list |> List.map bullet_of_json in
  { state_players = players;
    guns = guns;
    bullets = bullets; }

(* [lobby_of_json] takes [json] and builds a [lobby] out of it by
    getting the lobby id, player list in that lobby and storing
    it in a state record *)
let lobby_of_json json =
  let open Yojson.Basic.Util in
  let id = json |> member "id" |> to_int in
  let lobby_players =  json |> member "players" |> to_list |> List.map to_string in
  { lobby_id = id;
    lobby_players = lobby_players}

(* [lobbies_of_json] takes a list of lobbies and runs [lobby_of_json]
    on each of them *)
let lobbies_of_json json =
  let open Yojson.Basic.Util in
  json |> member "lobbies" |> to_list |> List.map lobby_of_json
