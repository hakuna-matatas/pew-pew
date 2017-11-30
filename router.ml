open Clo
open Lwt
open Cohttp
open Cohttp_lwt_unix

(******************************************************************************
   This router contains code for the GUI to deal with requests. In general
   each function deals with one of the calls from the API. The functions take
   the appropriate information and give back the appropriate state to the user.
 ******************************************************************************)

module Router = struct

  type request =
    | GetState
    | Move
    | Fire
    | Take
    | GetLobby of int

  type request_type =
    | Get
    | Post

  let root = ""

  let get_url id = function
    | GetState -> root ^ "getstate" ^ id
    | Move -> root ^ "move" ^ id
    | Fire -> root ^ "fire" ^ id
    | Take -> root ^ "take" ^ id
    | GetLobby lobby_id -> root ^ "getlobby" ^ id ^ string_of_int lobby_id

  let get_request_type = function
    | GetState -> Get
    | Move -> Post
    | Fire -> Post
    | Take -> Post
    | GetLobby _ -> Get
end


let make_newtwork_request id router map body =
  let open Router in
  let open Cohttp_lwt_unix in
  let url = get_url id router in
  let req_type = get_request_type router in
  match req_type with
  | Get -> Client.get (Uri.of_string url) >>= fun (resp, body) -> map resp body
  | Post -> Client.post ~body:(Cohttp_lwt_body.of_string body) (Uri.of_string url)
    >>=  fun (resp, body) -> map resp body

let make_get_request router map =
  make_newtwork_request router map

let make_post_request router map params =
  make_newtwork_request map params


(* [get_world_state] returns the current world state of the model*)
let get_world_state id = failwith "Unimplemented"

(* [move_location] tells the world where in which direction a player moved*)
let move_location  id direction = failwith "Unimplemented"

(* [fire] tells the world to fire a shot and for the user*)
let fire id = failwith "Unimplemented"

(* [take_item] takes a closeby item for the user *)
let take_item id = failwith "Unimplemented"

(* [get_lobbies] gets thec current lobbies in the game*)
let get_lobbies id = failwith "Unimplemented"

(* [get_lobbies] allows the user to create a lobby *)
let create_lobby id = failwith "Unimplemented"

(* [join_lobby] takes a player places them into a lobby_id *)
let join_lobby id lobby_id = failwith "Unimplemented"
