open State
open Server

let delay = 0.033

let sleep sec = 
	ignore (Unix.select [] [] [] sec)

let rec step lobby = 
	sleep delay; step lobby

let main () = 
	let _ = Thread.create run 8000 in step []

let () = main ()