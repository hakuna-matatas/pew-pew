open Ctype

(******************************************************************************
   This router contains code for the GUI to deal with requests. In general
   each function deals with one of the calls from the API. The functions take
   the appropriate information and give back the appropriate state to the user.
 ******************************************************************************)

(* [get_world_state game_id player_id callback] applies callback to 
 * the current world state of [game_id] from the perspective of [player_id].  *)
val get_world_state: id -> id -> (state -> 'a) -> 'a

(* [get_lobbies callback] applies callback to a list of
 * existing games. *)
val get_lobbies: (description list -> 'a) -> 'a

(* [move_location game_id player_id pos callback] attempts to move [player_id] to
 * [pos] in [game_id]. *)
val move_location:  id -> id -> pos -> (state -> 'a) -> 'a

(* [fire game_id player_id gun_id callback] attempts to fire [player_id]'s
 * [gun_id] in [game_id]. *)
val fire: id -> id -> id -> (state -> 'a) -> 'a

(* [get_lobbies game_name player_name callback] creates [game_name] and
 * inserts [player_name], *)
val create_lobby: name -> name -> (create_response -> 'a) -> 'a

(* [join_lobby game_id player_name callback] takes a player places 
 * them into the existing [game_id]. *)
val join_lobby: id -> name -> (id -> 'a) -> 'a
