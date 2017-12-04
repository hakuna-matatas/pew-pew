open State

type t

val create : State.t -> t
    
val update : t -> State.entity -> unit

val remove : t -> State.entity -> unit

val all : (State.entity * State.entity) list
