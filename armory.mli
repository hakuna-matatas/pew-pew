(* Module for defining and dispensing new weapon types. *)
type t

val create : unit -> t

val create_guns : t -> int -> Type.gun list

val create_ammo : t -> int -> Type.ammo list
