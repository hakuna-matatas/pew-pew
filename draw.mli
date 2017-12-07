(* [draw_state st pid gun] draws the state of the world
    as represented by [st]. [pid] and [gun] is the player id
    and gun name, respectively. *)
val draw_state: Ctype.state -> Ctype.id -> Ctype.name -> Ctype.dir -> unit
