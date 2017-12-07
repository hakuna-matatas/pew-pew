open Lwt
open Cohttp
open Cohttp_lwt_unix
open State

let lobby = ref []

(* STATE API COMMANDS *)
let get_st req = failwith "todo"

(* LOBBY API COMMANDS *)
let lobby_to_json lobby_id st =
	`Assoc [
		("name", 	  `String lobby_id);
		("players", `List   (List.map (fun x -> `String x) (State.players st)))
	]

let get_lobbyID req =
	req.req_body 
	|> Yojson.Basic.from_string 
	|> member "name" 
	|> Yojson.Basic.Util.to_string

let get_lobbies _ = 
	let body = `List (List.map lobby_to_json lobby) in
	Server.respond_string ~status: `OK ~body ()

let create_game req =
	try 
		let lobby_id = get_lobbyID req in
		let player = List.assoc "player_id" req.params in
		lobby := (lobby_id, State.add_player player State.empty) :: !lobby;
		Server.respond_string ~status: `Created ~body:"" ()
	with
		Not_found -> failwith "Cannot find player_id in req params"
	 
let rec add_player lobby lobby_id pid =
	match lobby with
	| [] 			 		-> failwith "Cannot find lobby"
	| (lid,st)::t ->
		if lid = lobby_id 
		then (lid, State.add_player pid st) :: t
		else (lid,st) :: add_player lobby_id pid  

let join_game req =
	let lobby_id = get_lobbyID req in
	let player = List.assoc "player_id" req.params in
	lobby := add_player !lobby lobby_id player;
	Server.respond_string ~status: `Created ~body:"" ()

(* PLAYER API COMMANDS *)
let get_pid_and_st req =
	let lobby_id = get_lobbyID req in
	let player = List.assoc "player_id" req.params in
	let st = List.assoc lobby_id !lobby in (player, st)

let shoot req = failwith "todo"

let move req = failwith "todo"


(* A list of all callbacks. Each callback takes in
	 a request and returns a server response. There
	 is a callback for each API command. *)
let responses = [
	("/lobby",	 "GET"),  get_st;
	("/move", 	 "POST"), move;
	("/shoot",	 "POST"), shoot;
	("/lobbies", "GET"),  get_lobbies;
	("/create",  "POST"), create_game;
	("/join",    "POST"), join_game;
]

(* Called whenever the server receives a request. *)
let callback responses _conn req body =
	let uri = req |> Request.uri in
	let meth = req |> Request.meth |> Code.string_of_method in
	let headers = req |> Request.headers |> Header.to_string in
		try
			body |> Cohttp_lwt.Body.to_string |> List.assoc (Uri.path uri,meth) responses
		with
		| Not_found -> failwith "invalid URI"

(* Runs the server on [port]. *)
let run ?(port = 8080) =
	let server = 
		Server.create ~mode:(`TCP (`Port port)) (Server.make (callback responses) ()) 
	in
	ignore (Lwt_main.run server)
