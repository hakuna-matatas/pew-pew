open Clo

(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)

(* [state] is an abstract type representing the state of a game. *)
type state 

val to_json_string: state -> string

(* [add_player id st] adds the player with username [id] to the game

	requires: [id] is a unique string representing the name of the
						 player to be added *)
val add_player: id -> state -> state

(* [remove_player id st] removes the player with username [id] from the game 
	 
	 requires: [id] is the username of a player in the game. *) 
val remove_player: id -> state -> state

