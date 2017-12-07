(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)
open Stype

(* [state] is an abstract type representing the state of a game. *)
type t

(* [create game_id game_name player_name] returns a tuple [(state, player_id)]. *)
val create : id -> name -> name -> (t * id)

(* [fire st p_id g_id] attempts to fire player [p_id]'s gun [g_id]. *)
val fire : t -> id -> id -> unit

(* [move st p_id pos] attempts to move player [p_id] to position pos. *)
val move : t -> id -> pos -> unit

val get_lock : t -> Mutex.t

(* [to_json_string st p_id] is the JSON string representation of [st] from player
 * [p_id]'s perspective as defined by the API. *)
val to_json_string: t -> id -> string

(* [to_description st] is the JSON representation of a game description
 * as defined by the API. *)
val to_description: t -> Yojson.Basic.json

(* [to_list st] is the list of all entities in [st]. *)
val to_list: t -> entity list

(* [create_player name st] adds the player with username [name] to the game, and
 * returns a unique ID. *)
val create_player: t -> name -> id

(* [step st] increments the game state by one tick. *)
val step: t -> unit
