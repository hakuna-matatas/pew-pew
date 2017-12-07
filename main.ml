open State
open Server

let delay = 0.033

let sleep sec = ignore (Unix.select [] [] [] sec)
(* 
let rec step lobby = sleep delay; step (State.step lobby) *)

let _ = Thread.create run 8080 (* in failwith "sad" *)