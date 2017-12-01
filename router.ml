open Clo
open Lwt
open Cohttp
open Cohttp_lwt_unix

(******************************************************************************
   This router contains code for the GUI to deal with requests. In general
   each function deals with one of the calls from the API. The functions take
   the appropriate information and give back the appropriate state to the user.
 ******************************************************************************)

(* Handles serializing data into JSON format so it is easy to convert
   maintain.
*)
module JsonHandler = struct
  open Yojson

  let json_of_dir: direction -> Yojson.json = failwith "Unimplemented"

  let json_of_fire: unit -> Yojson.json = failwith "Unimplemented"

  let json_of_take: unit -> Yojson.json = failwith "Unimplemented"

  let json_of_join: unit -> Yojson.json = failwith "Unimplemented"

  let json_of_create: unit -> Yojson.json = failwith "Unimplemented"

end


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
    | Take
    | CreateLobby
    | JoinLobby

  type request_type =
    | Get
    | Post of Yojson.json

  (* The exception that will be raised if a post request does not have a body *)
  exception NoPostBody

  (* Ensures a post object has a body *)
  let bind_post m =
      match m with
      | Some x -> x
      | None -> raise NoPostBody

  let root = ""

  let get_url id = function
    | GetState -> root ^ "getstate" ^ id
    | Move -> root ^ "move" ^ id
    | Fire -> root ^ "fire" ^ id
    | Take -> root ^ "take" ^ id
    | GetLobby -> root ^ "getlobby" ^ id
    | CreateLobby -> root ^ "create_lobby" ^ id
    | JoinLobby -> root ^ "join_lobby"  ^ id

  let get_request_type router body  =
    match router with
    | GetState -> Get
    | GetLobby -> Get
    | Move -> Post (bind_post body)
    | Fire -> Post (bind_post body)
    | Take -> Post (bind_post body)
    | CreateLobby -> Post (bind_post body)
    | JoinLobby -> Post (bind_post body)

end

(******************************************************************************
   Helper Functions
 ******************************************************************************)

open Router

(* [make_newtwork_request] takes the [id] and the type of the
     request [router] and attempts to make a network request for the user.
     The [map] function and deals with the response of the http function.
     The [callback] function is what happens after the response is returned.
     It takes in the type that the [map] function returns.
     [body] is an optional parameter that is necessary for post requests.
     An exception is thrown if a post request does not have a body. *)
let make_newtwork_request
    (id:string) (router:Router.request) ?body (map: 'a -> 'b) (callback: 'b -> 'c)  =
  let open Cohttp_lwt_unix in
  let url = get_url id router in
  let req_type = get_request_type router body in
  let mapped_body =
  match req_type with
  | Get -> Client.get (Uri.of_string url) >|= map
  | Post body -> Client.post ~body:(Yojson.to_string body |> Cohttp_lwt_body.of_string) (Uri.of_string url)
    >|=  map in
  let body = Lwt_main.run mapped_body in
  callback body

(* [make_get_request] takes a [router] which specifies the type of the request
    then uses the map to transform the response of the get request.
    The [callback] then uses the mapped_response and can do anything with that.
    The [id] is the unique identification of the user.
    Precondition: The router type is a GET request. *)
let make_get_request id router map callback =
  make_newtwork_request id router map callback

(* [make_get_request] takes a [router] which specifies the type of the request
    then uses the [map] to transform the response of the get request.
    [The callback]then uses the mapped_response and can do anything with that.
    A [body] is also necessary for any post request.
    The [id] is the unique identification of the user.
    Precondition: The router type is a POST request. *)
let make_post_request id router body map callback =
  make_newtwork_request id router ~body:(body) map callback

(* [ident] returns itself. *)
let ident = (fun x -> x)

(******************************************************************************
   GET Requests
 ******************************************************************************)

(* [get_world_state] returns the current world state of the model*)
let get_world_state id callback =
  make_get_request id GetState ident callback

(* [get_lobbies] gets thec current lobbies in the game*)
let get_lobbies id callback =
  make_get_request id GetLobby ident callback


(******************************************************************************
   POST Requests
******************************************************************************)

open JsonHandler

(* [move_location] tells the world where in which direction a player moved*)
let move_location id dir callback =
  let json_body = json_of_dir dir in
  let router = Move in
  make_post_request id router json_body ident callback


(* [fire] tells the world to fire a shot and for the user*)
let fire id callback =
  let json_body = json_of_fire () in
  let router = Fire in
  make_post_request id router json_body ident callback

(* [take_item] takes a closeby item for the user *)
let take_item id callback =
  let json_body = json_of_take () in
  let router = Take in
  make_post_request id router json_body ident callback

(* [get_lobbies] allows the user to create a lobby *)
let create_lobby id callback =
  let json_body = json_of_create () in
  let router = CreateLobby in
  make_post_request id router json_body ident callback

(* [join_lobby] takes a player places them into a lobby_id *)
let join_lobby id lobby_id callback =
  let json_body = json_of_join () in
  let router = JoinLobby in
  make_post_request id router json_body ident callback
