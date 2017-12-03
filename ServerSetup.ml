open Lwt
open Cohttp
open Cohttp_lwt_unix


(* LOBBY *)


(* STATE *)


(* PLAYER *)


let responses = [("/hello","GET"),(fun _ -> Server.respond_string ~status:`OK ~body:"works :)" ())]

(* This is the method that's called whenever the server 
	 receives a request. *)
let callback responses _conn req body =
	let uri = req |> Request.uri in
	let meth = req |> Request.meth |> Code.string_of_method in
	let headers = req |> Request.headers |> Header.to_string in
		try
			body |> Cohttp_lwt.Body.to_string |> List.assoc (Uri.path uri,meth) responses
		with
		| Not_found -> failwith "invalid URI"


let run ?(port = 8080) responses =
	let server = 
		Server.create ~mode:(`TCP (`Port port)) (Server.make (callback responses) ()) 
	in
	ignore (Lwt_main.run server)
