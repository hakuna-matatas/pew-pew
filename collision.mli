open State

type t

val create : unit -> t
    
val update : State.entity -> t -> unit

val all : (State.entity * State.entity) list
