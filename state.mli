(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)

(* [state] is an abstract type representing the state of a game. *)
type state 

type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW

type g_id = string
type p_id = string
type r_id = int

type entity = 
| Rock   of (r_id * rad * pos) 
| Bullet of (g_id * rad * pos)
| Ammo   of (g_id * rad * pos)
| Gun    of (g_id * rad * pos)
| Player of (p_id * rad * pos)

val to_json_string: state -> string

(* [add_player id st] adds the player with username [id] to the game

	requires: [id] is a unique string representing the name of the
						 player to be added *)
val add_player: p_id -> state -> state

(* [remove_player id st] removes the player with username [id] from the game 
	 
	 requires: [id] is the username of a player in the game. *) 
val remove_player: p_id -> state -> state

