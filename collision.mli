open State

type t

(* Initializes a map [t] from the given state. *)
val create : State.t -> t
    
(* Updates an entity's radius and position in map [t].
 *
 * If entity does not exist, update will create it.
 * *)
val update : t -> State.entity -> unit

(* Removes an entity from the map [t]. *)
val remove : t -> State.entity -> unit

(* Returns a list of all collisions in map [t]. *)
val all : t -> (State.entity * State.entity) list

(* Determines if two entities collide. *)
val intersect : State.entity -> State.entity -> bool
