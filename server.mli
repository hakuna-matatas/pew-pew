(* Runs the server on port number [port] *)
val run: int -> unit

(*  *)
val get_lobby: unit -> (int * State.t) list ref