
(******************************************************************************
   These Client objects represent the data types that will be parsed for the
   client. These are the objects that the json objects will be parsed into
   and the parsed out of.
 ******************************************************************************)

 (* The [lobby_id] is the current lobby a player is in *)
type lobby_id

(* This is the [id] of a player in the world. This is unique for every player. *)
type id

(* The location of an entity in the game map *)
type location

(* This will be a variant with 8 directions (N, NW, W, SW, S, SE, E, NE *)
type direction

(* The is a variant of the different types of guns *)
type gun_type

(* The record holds the information for a gun inside the client *)
type gun

(* This record holds the information for a bullet inside the client *)
type bullet

(* This record holds the information for a player inside the client*)
type player

(* This type respresents what the http [response] can give back.*)
type state
