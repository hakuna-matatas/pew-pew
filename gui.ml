open Graphics
open Settings
open Clo

let w_string = " 500x500"
let screen_width = 500
let screen_height = 500
let map_width = int_of_float Settings.map_width
let map_height = int_of_float Settings.map_height

type pos = (int*int)

let player_pos = ref (50,50)
let rock_pos = (75,75)

type person = {
  id:string;
  name:string;
  hp:int;
  pos:int list;
  rad:int;
  dir:string;
  inv:string list
}

let get_tl_x px py =
  let half_screen = screen_width / 2 in
  if px < half_screen then 0
  else if px > map_width - half_screen then map_width - screen_width
  else px - (screen_width / 2)

let get_tl_y px py =
  let half_screen = screen_height / 2 in
  if py < half_screen then 0
  else if py > map_height - half_screen then map_height - screen_height
  else py - (screen_height / 2)

let corner_pos px py =
  (get_tl_x px py, get_tl_y px py)

(* Calculates the position of world elements
   relative to the top left corner of the screen *)
let get_rel_pos (elem_x, elem_y) =
  let (px, py) = !player_pos in
  let (tl_x, tl_y) = corner_pos px py in
  (elem_x - tl_x, elem_y - tl_y)

let draw_ammo a = failwith "todo"

let draw_bullets b = failwith "todo"

let draw_player p = failwith "todo"

let draw_guns g = failwith "todo"

let draw_rock rock =
  let (rx,ry) = get_rel_pos rock.pos in
  draw_circle rx ry rock.rad

let draw_rocks l =
  List.iter draw_rock l




let draw_move dir =
  clear_graph;
  set_color green;
  fill_rect 0 0 (size_y ()) (size_x ());
  set_color black;
  fill_rect 250 250 50 50
(*
  match dir with
  | `N ->
  | `E ->
  | `S ->
  | `W -> *)
(*
    let move_cb {_,_,_,key_pressed,key} =
      match key with
      | 'w' -> draw_move `N
      | 'a' -> draw_move `W
      | 's' -> draw_move `S
      | 'd' -> draw_move `E
      | ' ' -> failwith "todo" *)

let main () =
  open_graph w_string;
  set_window_title "Apex";

  set_color green;
  fill_rect 0 0 (size_y ()) (size_x ());

  set_color black;
  fill_rect 250 250 50 50 		(* Test player *)

  (* loop_at_exit [Key_pressed] move_cb *)

let () = main ()
