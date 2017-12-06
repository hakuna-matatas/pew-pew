(******************************************************************************
   This module has the code for the state.
 ******************************************************************************)

(* [state] is an abstract type representing the state of a game. *)
type t

(* [create id] returns a game with id [id]. *)
val create : Type.id -> t

(* [fire st p_id g_id] attempts to fire player [p_id]'s gun [g_id]. *)
val fire : t -> Type.id -> Type.id -> unit

(* [move st p_id pos] attempts to move player [p_id] to position pos. *)
val move : t -> Type.id -> Type.pos -> unit

(* [to_json_string st] is the JSON representation of [st] as defined by the API. *)
val to_json_string: t -> string

(* [to_list st] is the list of all entities in [st]. *)
val to_list: t -> Type.entity list

(* [create_player name st] adds the player with username [name] to the game, and
 * returns a unique ID. *)
val create_player: t -> Type.name -> unit

(* [destroy_player id st] removes the player with unique ID [id] from the game. *)
val destroy_player: t -> Type.id -> unit

(* [step st] increments the game state by one tick. *)
val step: t -> unit
