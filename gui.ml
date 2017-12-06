open Graphics
open Settings

let w_string = " 500x500"

type pos = {x:int; y:int}

let player_pos = ref {x=50;y=50}
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

(* let horizontally_fixed player_pos : bool =
  match player_pos with
  | [elem_x; elem_y] -> *)

(* Calculates the position of world elements
   relative to the top left corner of the screen *)
let get_rel_pos pos_lst =
  match pos_lst with
  | [elem_x; elem_y] ->
    let tl_corner_pos =
      let px = !player_pos.x in
      let py = !player_pos.y in
      {
        x = px - (size_x () / 2);
        y = py - (size_y () / 2)
      }
    in
    {
      x = elem_x - tl_corner_pos.x;
      y = elem_y - tl_corner_pos.y
    }
  | _ -> failwith "Invalid JSON"

let draw_ammo a = failwith "todo"

let draw_bullets b = failwith "todo"

let draw_player p = failwith "todo"

let draw_guns g = failwith "todo"

let draw_rocks r = failwith "Unimpplemented"
  (* let helper rock =
    let r_pos = get_rel_pos (rock.pos) in
    draw_circle r_pos.x r_pos.y rock.rad
      List.iter helper r *)



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
