let delay = 0.033

let sleep sec = 
	ignore (Unix.select [] [] [] sec)

let rec step () = 
	let states = List.map (fun (gid,st) -> st) !(Server.get_lobby ()) in
	sleep delay; 
	List.iter State.step states;
	step ()

let main () = 
	let _ = Thread.create Server.run 8000 in step ()

let () = main ()