open Graphics
open Settings
open Ctype
open Sdlevent
open Sdlkey

let player_pos = ref (1500,1500)
let rock_pos = (75,75)
let id = ref 0

let state_test = {
  id = 0;
  name = "test_state";
  size = (5000,5000);
  radius = 2000;
  ammo = [{a_type = "pistol";
           a_pos = (1200,1200);
           a_rad = 50;
           a_amt = 5;
          }];
  bullets = [{b_type = "pistol";
              b_pos = (1550,1550);
              b_rad = 5;}];
  players = [{p_id = 1;
              p_name = "Alan";
              p_hp = 5;
              p_pos = (1500,1500);
              p_rad = 20;
              p_dir = SE;
              p_inv = [10]}];
  guns = [{g_id = 10;
          g_own = 1;
          g_pos = (1250, 1250);
          g_rad = 10;
          g_type = "pistol";
          g_ammo = 10}];
  rocks = [{r_pos = (1340, 1340);
            r_rad = 3}];
}


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
  set_color green;
  fill_rect 0 0 client_height client_width;

  draw_state state_test;

  Sdl.init [`EVENTTHREAD; `VIDEO];
  Sdlkey.enable_key_repeat ?delay:(Some 0) ?interval:(Some 10) ();
  Sdlvideo.set_video_mode 1 1 [];

  loop state_test ()

let () = main ()
