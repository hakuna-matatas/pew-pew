open Clo

(******************************************************************************
   This router contains code for the GUI to deal with requests. In general
   each function deals with one of the calls from the API. The functions take
   the appropriate information and give back the appropriate state to the user.
 ******************************************************************************)

(* [get_world_state] returns the current world state of the model*)
val get_world_state: id -> response

(* [move_location] tells the world where in which direction a player moved*)
val move_location:  id -> direction -> response

(* [fire] tells the world to fire a shot and for the user*)
val fire: id -> response

(* [take_item] takes a closeby item for the user *)
val take_item: id -> response

(* [get_lobbies] gets thec current lobbies in the game*)
val get_lobbies: id -> response

(* [get_lobbies] allows the user to create a lobby *)
val create_lobby: id -> response

(* [join_lobby] takes a player places them into a lobby_id *)
val join_lobby: id -> lobby_id -> response
