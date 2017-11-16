
(******************************************************************************
   These Client objects represent the data types that will be parsed for the
   client. These are the objects that the json objects will be parsed into
   and the parsed out of.
 ******************************************************************************)

(* This type respresents what the http [response] can give back.*)
type response = unit

(* This is the [id] of a player in the world. This is unique for every player. *)
type id = unit

(* The [lobby_id] is the current lobby a player is in *)
type lobby_id = unit

(* This will be a variant with 8 directions (N, NW, W, SW, S, SE, E, NE *)
type direction = unit

(* The location of an entity in the game map *)
type location = unit