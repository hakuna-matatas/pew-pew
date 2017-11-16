open Clo

(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)

(* [state] is an abstract type representing the state of a game. *)
type state 

(* [init_state j] is the initial state of the game.

 * requires: [j] represents a JSON file containing all
						 the players and their information. *)
val init_state : Yojson.Basic.json -> state

(* [meta_info st] is a tuple of the name of the game, as well as
	 the number of players currently in it *)
val meta_info: state -> string*int

(* [add_player id st] adds the player with username [id] to the game

	requires: [id] is a unique string representing the name of the
						 player to be added *)
val add_player: id -> state -> state

(* [remove_player id st] removes the player with username [id] from the game 
	 
	 requires: [id] is the username of a player in the game. *) 
val remove_player: id -> state -> state

(* [take_item id st] takes the closest item to the player with username [id]
	 and places it in his inventory while removing it from the game world.
	 If there is no item within within 15 pixels of the player, then nothing
	 is picked up.

	 requires: [id] is the username of a player in the game. *)
val take_item: id -> state -> state

(* [shoot id st] fires a projectile originating from the location of the player
	 with username [id]. If the player has no gun, or has no more ammunition,
	 nothing happens. 

	 requires: [id] is the username of a player in the game. *)
val shoot: id -> state -> state

(* [move id dir st] changes the player's location in direction [dir] by 5
	 pixels. If the player is against a wall/object, the player's location will not
	 change.

	 requires: [id] is the username of a player in the game
	 					 [dir] is a valid direction *)
val move: id -> direction -> state -> state

(* [get_curr_item id st] is the current item player [id] is holding

	 requires: [id] is the username of a player in the game*)
val get_curr_item: id -> state -> string

(* [get_health id st] is the current health of player [id] 

	requires: [id] is the username of a player in the game*)
val get_health: id -> state -> int

(* [get_inv id st] is the current inventory of player [id] 

	 requires: [id] is the username of a player in the game*)
val get_inv: id -> state -> string list

(* [get_loc id st] is the current location of player [id] 

	 requires: [id] is the username of a player in the game*)
val get_loc: id -> state -> int * int

(* [get_dir id st] is the current direction of player [id] 

	 requires: [id] is the username of a player in the game*)
val get_dir: id -> state -> direction

(* [onscreen_elems id st] is a list of items/world elements and their locations.
	 Only elements that are within 1000 pixels of the player with username [id] 
	 should be returned.

	 requires: [id] is the username of a player in the game *)
val onscreen_elems: id -> state -> string * location list

(* [onscreen_players id st] is a list of player ids and their locations.
	 Only players that are within 1000 pixels of the player with username [id]
	 should be returned. 

	requires: [id] is the username of a player in the game *)
val onscreen_players: id -> state -> id * location list








