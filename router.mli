open Ctype

(******************************************************************************
   This router contains code for the GUI to deal with requests. In general
   each function deals with one of the calls from the API. The functions take
   the appropriate information and give back the appropriate state to the user.
 ******************************************************************************)

(* [get_world_state] returns the current world state of the model *)
val get_world_state: id -> int -> (Client.state -> 'a) -> 'a

(* [get_lobbies] gets thec current lobbies in the game *)
val get_lobbies: id -> (Client.description list -> 'a) -> 'a

(* [move_location] tells the world where in which direction a player moved*)
val move_location:  id -> int -> Client.pos -> (Client.state -> 'a) -> 'a

(* [fire] tells the world to fire a shot and for the user*)
val fire: id -> int -> int -> (Client.state -> 'a) -> 'a

(* [get_lobbies] allows the user to create a lobby *)
val create_lobby: string -> string -> (Yojson.Basic.json -> 'a) -> 'a

(* [join_lobby] takes a player places them into a lobby_id *)
val join_lobby: int -> int -> (Yojson.Basic.json -> 'a) -> 'a
