open Graphics
open Settings
open Client
open Sdlevent
open Sdlkey

let w_string = " 500x500"
let screen_width = 500
let screen_height = 500
let map_width = int_of_float Settings.map_width
let map_height = int_of_float Settings.map_height

type pos = (int*int)

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

let get_bl_x px py =
  let half_screen = screen_width / 2 in
  if px < half_screen then 0
  else if px > map_width - half_screen then map_width - screen_width
  else px - (screen_width / 2)

let get_bl_y px py =
  let half_screen = screen_height / 2 in
  if py < half_screen then 0
  else if py > map_height - half_screen then map_height - screen_height
  else py - (screen_height / 2)

let corner_pos px py =
  (get_bl_x px py, get_bl_y px py)

(* Calculates the position of world elements
   relative to the top left corner of the screen *)
let get_rel_pos (elem_x, elem_y) =
  let (px, py) = !player_pos in
  let (tl_x, tl_y) = corner_pos px py in
  (elem_x - tl_x, elem_y - tl_y)

let get_gcolor = function
  | "pistol" -> set_color red
  | _        -> set_color black

let draw_ammo a =
  let (ax, ay) = get_rel_pos a.a_pos in
  get_gcolor a.a_type;
  fill_circle ax ay a.a_rad

let draw_ammo a =
  List.iter draw_ammo a

let draw_bullet b =
  let (bx,by) = get_rel_pos b.b_pos in
  get_gcolor b.b_type;
  fill_circle bx by b.b_rad

let draw_bullets b = List.iter draw_bullet b

let draw_player p =
  let (px,py) = get_rel_pos p.p_pos in
  set_color blue;
  fill_circle px py p.p_rad

let draw_players p =
  List.iter draw_player p

let draw_gun g =
  let (gx,gy) = get_rel_pos g.g_pos in
  get_gcolor g.g_type;
  fill_circle gx gy g.g_rad

let draw_guns g = List.iter draw_gun g

let draw_rock rock =
  let (rx,ry) = get_rel_pos rock.r_pos in
  set_color black;
  fill_circle rx ry rock.r_rad

let draw_rocks r =
  List.iter draw_rock r

let draw_state
    {id;name;size;radius;ammo;bullets;players;guns;rocks} =
  draw_ammo ammo;
  draw_bullets bullets;
  draw_players players;
  draw_guns guns;
  draw_rocks rocks

let get_direction () =
  if is_key_pressed KEY_w && is_key_pressed KEY_a then Some Type.NW else
  if is_key_pressed KEY_w && is_key_pressed KEY_d then Some Type.NE else
  if is_key_pressed KEY_s && is_key_pressed KEY_d then Some Type.SE else
  if is_key_pressed KEY_s && is_key_pressed KEY_a then Some Type.SW else
  if is_key_pressed KEY_w then Some Type.N else
  if is_key_pressed KEY_d then Some Type.E else
  if is_key_pressed KEY_s then Some Type.S else
  if is_key_pressed KEY_a then Some Type.W else None

let move s d = 
  let (x, y) = !player_pos in 
  let cps  = client_player_speed in
  let cps' = int_of_float (sqrt 2. *. (float_of_int cps) /. 2.) in
  let _ = match d with
  | Type.NW -> player_pos := (x - cps' , y + cps')
  | Type.NE -> player_pos := (x + cps' , y + cps')
  | Type.SE -> player_pos := (x + cps' , y - cps')
  | Type.SW -> player_pos := (x - cps' , y - cps')
  | Type.N  -> player_pos := (x , y + cps)
  | Type.E  -> player_pos := (x + cps , y)
  | Type.S  -> player_pos := (x , y - cps)
  | Type.W  -> player_pos := (x - cps , y) in
  clear_graph (); draw_state s

let rec loop s () =
  Unix.sleepf 0.02;
  let _ = match get_direction () with
  | None   -> ()
  | Some d -> move s d in
  loop s ()

let main () =
  open_graph w_string;
  set_window_title "Apex";
  set_color green;
  fill_rect 0 0 screen_height screen_width;

  draw_state state_test;

  Sdl.init [`EVENTTHREAD; `VIDEO];
  Sdlkey.enable_key_repeat ?delay:(Some 0) ?interval:(Some 10) ();
  Sdlvideo.set_video_mode 1 1 [];

  loop state_test ()

let () = main ()
