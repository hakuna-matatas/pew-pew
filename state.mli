(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)

(* [state] is an abstract type representing the state of a game. *)
type t

type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW
type id  = string

type entity = 
| Rock   of (id * rad * pos) 
| Bullet of (id * rad * pos)
| Ammo   of (id * rad * pos)
| Gun    of (id * rad * pos)
| Player of (id * rad * pos)

val to_json_string: t -> string

val to_list: t -> entity list

(* [add_player id st] adds the player with username [id] to the game

	requires: [id] is a unique string representing the name of the
						 player to be added *)
val add_player: id -> t -> t

(* [remove_player id st] removes the player with username [id] from the game 
	 
	 requires: [id] is the username of a player in the game. *) 
val remove_player: id -> t -> t

