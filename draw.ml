open Graphics
open Settings
open Ctype

let get_bl_x px py =
  let half_screen = client_width / 2 in
  if px < half_screen then 0
  else if px > int_of_float map_width - half_screen then int_of_float map_width - client_width
  else px - (client_width / 2)

let get_bl_y px py =
  let half_screen = client_height / 2 in
  if py < half_screen then 0
  else if py > int_of_float map_height - half_screen then int_of_float map_height - client_height
  else py - (client_height / 2)

let corner_pos px py =
  (get_bl_x px py, get_bl_y px py)

(* Calculates the position of world elements
   relative to the bottom left corner of the screen *)
let get_rel_pos (elem_x, elem_y) (px, py) =
  let (tl_x, tl_y) = corner_pos px py in
  (elem_x - tl_x, elem_y - tl_y)

let get_gcolor = function
  | "Pistol"  -> set_color red
  | "Octo"    -> set_color blue
  | "Shotgun" -> set_color green
  | _         -> set_color black

let draw_ammo ppos a =
  let (ax, ay) = get_rel_pos a.a_pos ppos in
  get_gcolor a.a_type;
  fill_rect ax ay (int_of_float ammo_radius) (int_of_float ammo_radius)

let draw_ammo a ppos =
  List.iter (draw_ammo ppos) a

let draw_bullet ppos b =
  let (bx,by) = get_rel_pos b.b_pos ppos in
  get_gcolor b.b_type;
  fill_circle bx by b.b_rad

let draw_bullets b ppos =
  List.iter (draw_bullet ppos) b

let draw_player ppos p =
  let (px,py) = get_rel_pos p.p_pos ppos in
  set_color blue;
  fill_circle px py p.p_rad

let draw_players p ppos =
  List.iter (draw_player ppos) p

let draw_gun ppos g =
  let (gx,gy) = get_rel_pos g.g_pos ppos in
  get_gcolor g.g_type;
  fill_circle gx gy g.g_rad

let draw_guns g ppos =
  List.iter (draw_gun ppos) g

let draw_rock ppos rock =
  let (rx,ry) = get_rel_pos rock.r_pos ppos in
  set_color black;
  fill_circle rx ry rock.r_rad

let draw_rocks r ppos =
  List.iter (draw_rock ppos) r

let line_len = 3

let dir_to_string d = match d with
  | N -> "north"
  | S -> "south"
  | E -> "E"
  | W -> "W"
  | NE -> "NE"
  | NW -> "NW"
  | SE -> "SE"
  | SW -> "SW"

let draw_curr_gun px py pr dir =
  let (x, y) = get_rel_pos (px,py) (px,py) in
  let (x', y') = match dir with
  | N  -> x, y + pr + line_len
  | S  -> x, y - pr - line_len
  | E  -> x + pr + line_len, y
  | W  -> x - pr - line_len, y
  | NE -> x + pr + line_len, y + pr + line_len
  | NW -> x - pr - line_len, y + pr + line_len
  | SE -> x + pr + line_len, y - pr - line_len
  | SW -> x - pr - line_len, y - pr - line_len
  in
  set_line_width 5;
  moveto x y;
  lineto x' y'

let (hp_x, hp_y) = (15,15)

let draw_hud p d gun =
  let (px, py) = p.p_pos in
  let hp = p.p_hp |> string_of_int in
  set_text_size 20;
  draw_string ("HP: " ^ hp);
  get_gcolor gun;
  draw_curr_gun px py p.p_rad d

let draw_border (px, py) =
  let (rx, ry) = get_rel_pos (0,0) (px, py) in
  set_line_width 10;
  set_color red;
  draw_rect rx ry (int_of_float map_width) (int_of_float map_height) 

let draw_state
    {id;name;size;radius;ammo;bullets;players;guns;rocks}
    p_id gun d =
  let p = List.find (fun p -> p.p_id = p_id) players in
  let ppos = p.p_pos in
  clear_graph ();
  draw_ammo ammo ppos;
  draw_bullets bullets ppos;
  draw_players players ppos;
  draw_guns (List.filter (fun g -> g.g_own = no_owner) guns) ppos;
  draw_rocks rocks ppos;
  draw_hud p d gun;
  draw_border ppos
