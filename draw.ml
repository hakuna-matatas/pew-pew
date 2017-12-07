open Graphics
open Settings
open Ctype

let get_bl_x px py =
  let half_screen = client_width / 2 in
  if px < half_screen then 0
  else if px > map_width - half_screen then map_width - client_width
  else px - (client_width / 2)

let get_bl_y px py =
  let half_screen = client_height / 2 in
  if py < half_screen then 0
  else if py > map_height - half_screen then map_height - client_height
  else py - (client_height / 2)

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

let line_len = 3

let draw_curr_gun px py pr dir =
  let (x', y') = match dir with
  | N  -> px, py + pr + line_len
  | S  -> px, py - pr - line_len
  | E  -> px + pr + line_len, py
  | W  -> px - pr - line_len, py
  | NE -> px + pr + line_len, py + pr + line_len
  | NW -> px - pr - line_len, py + pr + line_len
  | SE -> px + pr + line_len, py - pr - line_len
  | SW -> px - pr - line_len, py - pr - line_len
  in
  set_line_width 5;
  moveto px py;
  lineto x' y'

let draw_hud pid gun players =
  let p = List.find (fun p' -> p' = pid) players in
  let (px, py) = get_rel_pos p.p_pos in
  let hp = p.p_hp |> string_of_int in
  set_text_size 20;
  draw_string ("HP: " ^ hp);
  get_gcolor gun;
  draw_curr_gun px py p.p_rad p.p_dir

let draw_state
    {id;name;size;radius;ammo;bullets;players;guns;rocks}
    pid gun =
  draw_ammo ammo;
  draw_bullets bullets;
  draw_players players;
  draw_guns guns;
  draw_rocks rocks;
  draw_hud pid gun players
