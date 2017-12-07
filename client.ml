open Graphics
open Settings
open Ctype
open Router
open Sdlevent
open Sdlkey

type t = {
  p_id        : id;
  g_id        : id;
  mutable pos : pos;
}

let create g_id p_id = {
  p_id;
  g_id;
  pos = (0, 0);
}

let to_pos st game =
  let p = List.find (fun p' -> p'.p_id = st.p_id) game.players in p.p_pos

let get_direction () =
  if is_key_pressed KEY_w && is_key_pressed KEY_a then Some NW else
  if is_key_pressed KEY_w && is_key_pressed KEY_d then Some NE else
  if is_key_pressed KEY_s && is_key_pressed KEY_d then Some SE else
  if is_key_pressed KEY_s && is_key_pressed KEY_a then Some SW else
  if is_key_pressed KEY_w then Some N else
  if is_key_pressed KEY_d then Some E else
  if is_key_pressed KEY_s then Some S else
  if is_key_pressed KEY_a then Some W else None

let move s d =
  let (x, y) = !player_pos in
  let cps  = client_player_speed in
  let cps' = int_of_float (sqrt 2. *. (float_of_int cps) /. 2.) in
  let _ = match d with
  | NW -> player_pos := (x - cps' , y + cps')
  | NE -> player_pos := (x + cps' , y + cps')
  | SE -> player_pos := (x + cps' , y - cps')
  | SW -> player_pos := (x - cps' , y - cps')
  | N  -> player_pos := (x , y + cps)
  | E  -> player_pos := (x + cps , y)
  | S  -> player_pos := (x , y - cps)
  | W  -> player_pos := (x - cps , y) in
  clear_graph (); draw_state s

let rec loop s () =
  Unix.sleepf 0.02;
  let _ = match get_direction () with
  | None   -> ()
  | Some d -> move s d in
  loop s ()

let main () =
  open_graph (" " ^ (string_of_int client_width) ^ "x" ^ (string_of_int client_height));
  set_window_title "Apex";

  Sdl.init [`EVENTTHREAD; `VIDEO];
  Sdlkey.enable_key_repeat ?delay:(Some 0) ?interval:(Some 10) ();
  Sdlvideo.set_video_mode 1 1 [];

  loop state_test ()

let () = main ()
