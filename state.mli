(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)

(* [state] is an abstract type representing the state of a game. *)
type t

(* TODO *)
val create : Type.id -> t

(* [to_json_string st] is the JSON representation of [st] as defined by the API. *)
val to_json_string: t -> string

(* [to_list st] is the list of all entities in [st]. *)
val to_list: t -> Type.entity list

(* [add_player id st] adds the player with username [id] to the game

	requires: [id] is a unique string representing the name of the
						 player to be added *)
val add_player: t -> Type.id -> unit

(* [remove_player id st] removes the player with username [id] from the game 
	 
	 requires: [id] is the username of a player in the game. *) 
val remove_player: t -> Type.id -> unit

(* [step st] increments the game state by one tick. *)
val step: t -> unit
