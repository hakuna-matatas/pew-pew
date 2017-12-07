(******************************************************************************
   This module has the code for the GUI.
 ******************************************************************************)
type t

(* [create g_id p_id state] creates a client for player [p_id] in game [g_id]. *)
val create : Ctype.id -> Ctype.id -> t

(* This is the main method of the GUI and initializes the application. *)
val run: t -> unit
