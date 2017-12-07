open Client
open Type
open Lwt
open Cohttp
open Cohttp_lwt_unix

(******************************************************************************
   This router contains code for the GUI to deal with requests. In general
   each function deals with one of the calls from the API. The functions take
   the appropriate information and give back the appropriate state to the user.
 ******************************************************************************)

(* The [Router] module essentially defines the requests that can be made, and
    what type of request it is (i.e. GET, POST, etc.) and the corresponding
    URL address. Each request always needs an id, but the POST requests need
    a body and the GetLobby request needs a lobby_id. *)
module Router = struct

  type request =
    | GetState
    | GetLobby
    | Move
    | Fire
    | CreateLobby
    | JoinLobby

  type request_type =
    | Get
    | Post of Yojson.Basic.json

  (* The exception that will be raised if a post request does not have a body *)
  exception NoPostBody

  (* Ensures a post object has a body *)
  let bind_post m =
      match m with
      | Some x -> x
      | None -> raise NoPostBody

  let root = ""

  let get_url id game_id gun_id router =
    let id = string_of_int id in
    match router with
    | GetState -> root ^ "/game" ^ string_of_int game_id ^ id
    | Move -> root ^ "/move" ^ string_of_int game_id ^ id
    | Fire -> root ^ "fire" ^ id ^ string_of_int game_id ^ string_of_int gun_id
    | GetLobby -> root ^ "getlobby" ^ id
    | CreateLobby -> root ^ "create_lobby" ^ id
    | JoinLobby -> root ^ "join_lobby"  ^ id

  let get_request_type router body  =
    match router with
    | GetState -> Get
    | GetLobby -> Get
    | Move -> Post (bind_post body)
    | Fire -> Get
    | CreateLobby -> Post (bind_post body)
    | JoinLobby -> Post (bind_post body)

end

(******************************************************************************
   Helper Functions
 ******************************************************************************)

open Router

(* [resp_of_json] takes a (code,resp) from the Cohttp libary and converts the
    response to a Json object. *)
let json_of_resp (code, resp) =
  Cohttp_lwt.Body.to_string resp >|=
    (fun s -> Yojson.Basic.from_string s)

let resp_of_json body =
  Yojson.Basic.to_string body |>  Cohttp_lwt.Body.of_string

(* [make_newtwork_request] takes the [id] and the type of the
     request [router] and attempts to make a network request for the user.
     The [map] function and deals with the response of the http function.
     The [callback] function is what happens after the response is returned.
     It takes in the type that the [map] function returns.
     [body] is an optional parameter that is necessary for post requests.
     An exception is thrown if a post request does not have a body. *)
let make_newtwork_request
    id game_id gun_id router ?body (map: 'a -> 'b) (callback: 'b -> 'c)  =
  let open Cohttp_lwt_unix in
  let url = get_url id game_id gun_id router in
  let req_type = get_request_type router body in
  let mapped_body =
  match req_type with
  | Get -> Client.get (Uri.of_string url) >>= json_of_resp >|= map
  | Post body -> Client.post ~body:(resp_of_json body) (Uri.of_string url)
    >>=  json_of_resp >|= map in
  let body = Lwt_main.run mapped_body in
  callback body

(* [make_get_request] takes a [router] which specifies the type of the request
    then uses the map to transform the response of the get request.
    The [callback] then uses the mapped_response and can do anything with that.
    The [id] is the unique identification of the user.
    Precondition: The router type is a GET request. *)
let make_get_request id game_id gun_id router map callback =
  make_newtwork_request id game_id gun_id router map callback

(* [make_get_request] takes a [router] which specifies the type of the request
    then uses the [map] to transform the response of the get request.
    [The callback]then uses the mapped_response and can do anything with that.
    A [body] is also necessary for any post request.
    The [id] is the unique identification of the user.
    Precondition: The router type is a POST request. *)
let make_post_request id game_id router body map callback =
  make_newtwork_request id game_id 0 router ~body map callback

(* [ident] returns itself. *)
let ident = (fun x -> x)

(******************************************************************************
   Game Requests
 ******************************************************************************)

(* [get_world_state] returns the current world state of the model*)
let get_world_state id game_id (callback) =
  make_get_request id game_id 0 GetState state_of_json callback

  (* [fire] tells the world to fire a shot and for the user*)
let fire id game_id gun_id callback =
  make_get_request id game_id gun_id Fire state_of_json callback

(* [move_location] tells the world where in which direction a player moved*)
let move_location id game_id (x,y) callback =
  let body =  `List [`Float (float_of_int x); `Float (float_of_int y)] in
  make_post_request id game_id Move body state_of_json callback


(******************************************************************************
   Lobby Requests
******************************************************************************)

(* [get_lobbies] gets thec current lobbies in the game*)
let get_lobbies id (callback) =
  make_get_request id 0 0 GetLobby description_of_json callback


(* [get_lobbies] allows the user to create a lobby *)
let create_lobby player_name game_name callback =
  let json_body =    `Assoc [
      ("game_name"   , `String player_name);
      ("player_name" , `String game_name);
    ] in
  make_post_request 0 0 CreateLobby json_body ident callback

(* [join_lobby] takes a player places them into a lobby_id *)
let join_lobby id lobby_id callback =
  make_get_request id lobby_id 0 JoinLobby ident callback
