open Graphics ;;
open Array ;;

let x_size= 500;;

let y_size= 500;;

let lobby_size= 12;;

let lobby = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40];;

let username= ref "";;

let lobbyname= ref "";;

let rec first_n n lst= match lst with
  | []-> []
  | h::t-> if n=0 then [] else h::(first_n (n-1) t);;

let prev = ref [];;

let current = ref (first_n lobby_size lobby);;

let next = ref (List.rev (first_n (List.length lobby - List.length !current) (List. rev lobby)));;

let draw_grid x_size y_size= open_graph (" "^(string_of_int x_size)^"x"^(string_of_int y_size));
  set_line_width 8; set_color black; set_text_size 30;
  moveto 0 0; lineto 0 y_size;
  moveto 0 0; lineto x_size 0;
  moveto (x_size/4) 0; lineto (x_size/4) y_size;
  moveto (x_size/2) (y_size/4); lineto (x_size/2) y_size;
  moveto (x_size*3/4) 0; lineto (x_size*3/4) y_size;
  moveto x_size 0; lineto x_size y_size;
  moveto 0 (y_size/4); lineto x_size (y_size/4);
  moveto 0 (y_size/2); lineto x_size (y_size/2);
  moveto 0 (y_size*3/4); lineto x_size (y_size*3/4);
  moveto 0 y_size; lineto x_size y_size;
  let ids= Array.of_list !current in

  let counter= ref (length ids) in

  if !counter< 0 then counter:=0;

  (if !counter <> 0 then
   moveto 15 (y_size*7/8);
   draw_string (string_of_int (get ids 0)); decr counter;

   if !counter <> 0 then
   moveto (x_size/4+15) (y_size*7/8);
   draw_string (string_of_int (get ids 1)); decr counter;

   if !counter <> 0 then
     (moveto (x_size/2+15) (y_size*7/8);
      draw_string (string_of_int (get ids 2)); decr counter);

   if !counter <> 0 then
     (moveto (x_size*3/4+15) (y_size*7/8);
      draw_string (string_of_int (get ids 3)); decr counter);

   if !counter <> 0 then
     (moveto (15) (y_size*5/8);
      draw_string (string_of_int (get ids 4)); decr counter);

   if !counter <> 0 then
     (moveto (x_size/4+15) (y_size*5/8);
      draw_string (string_of_int (get ids 5)); decr counter);

   if !counter <> 0 then
     (moveto (x_size/2+15) (y_size*5/8);
      draw_string (string_of_int (get ids 6)); decr counter);

   if !counter <> 0 then
     (moveto (x_size*3/4+15) (y_size*5/8);
      draw_string (string_of_int (get ids 7)); decr counter);

   if !counter <> 0 then
     (moveto (15) (y_size*3/8);
      draw_string (string_of_int (get ids 8)); decr counter);

   if !counter <> 0 then
     (moveto (x_size/4+15) (y_size*3/8);
      draw_string (string_of_int (get ids 9)); decr counter);

   if !counter <> 0 then
     (moveto (x_size/2+15) (y_size*3/8);
      draw_string (string_of_int (get ids 10)); decr counter);

   if !counter <> 0 then
     (moveto (x_size*3/4+15) (y_size*3/8);
      draw_string (string_of_int (get ids 11)); decr counter);
   moveto (15) (y_size*1/8);
   draw_string "Previous";
   moveto (15 + x_size*3/4) (y_size*1/8);
   draw_string "Next";
   moveto (x_size*1/2 -20) (y_size*1/8);
   draw_string "Create lobby";
   moveto 5 5;
   draw_string ("Username: "^(!username));
  ) in ();

let grid_number x y= match ((float_of_int x) /. (float_of_int x_size),
                            (float_of_int y) /. (float_of_int y_size)) with
                     | (a,b) -> if a<=0.25 then
                         (if b<=0.25 then 13
                         else if b<=0.5 then 9
                         else if b<=0.75 then 5
                         else 1)

                       else if a<=0.5 then
                         (if b<=0.25 then 14
                          else if b<=0.5 then 10
                          else if b<=0.75 then 6
                          else 2)

                       else if a<=0.75 then
                         (if b<=0.25 then 14
                          else if b<=0.5 then 11
                          else if b<=0.75 then 7
                          else 3)

                       else (if b<=0.25 then 15
                             else if b<=0.5 then 12
                             else if b<=0.75 then 8
                             else 4) in

let rec mouse_pos () =
  let event = wait_next_event [Button_down] in
  let ids= Array.of_list !current in
  let grid_num= grid_number event.mouse_x event.mouse_y in
  (match grid_num with
   |1-> print_string "Button "; print_int (get ids (grid_num-1));
   |2-> print_string "Button "; print_int (get ids (grid_num-1));
   |3-> print_string "Button "; print_int (get ids (grid_num-1));
   |4-> print_string "Button "; print_int (get ids (grid_num-1));
   |5-> print_string "Button "; print_int (get ids (grid_num-1));
   |6-> print_string "Button "; print_int (get ids (grid_num-1));
   |7-> print_string "Button "; print_int (get ids (grid_num-1));
   |8-> print_string "Button "; print_int (get ids (grid_num-1));
   |9-> print_string "Button "; print_int (get ids (grid_num-1));
   |10-> print_string "Button "; print_int (get ids (grid_num-1));
   |11-> print_string "Button "; print_int (get ids (grid_num-1));
   |12-> print_string "Button "; print_int (get ids (grid_num-1));
   |13-> if List.length !prev > 0 then (next:= (!current @ !next);
     current:= (List.rev (first_n lobby_size (List.rev !prev)));
     prev:= first_n (List.length !prev - List.length !current) !prev;
     draw_grid x_size y_size);
   |14-> ((print_string "Creating lobby");
     let lob_paint= open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
       moveto 0 (x_size/2); set_color black;
       draw_string "Enter lobbyname. Press enter to send.";
       moveto 0 (x_size/3); draw_string !lobbyname; in

     let rec lob_interactive () =
       let event = wait_next_event [Key_pressed] in
       print_char event.key; print_newline ();
       if event.key == '\b' then ((if String.length (!lobbyname) > 0
                                   then lobbyname := (String.sub (!lobbyname) 0 ((String.length !lobbyname) -1))); (print_string !lobbyname;
                                    open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
                                    moveto 0 (x_size/2); set_color black;
                                    draw_string "Enter lobbyname. Press enter to send.";
                                    moveto 0 (x_size/3); draw_string !lobbyname;
                                    lob_interactive ());)
       else if event.key == '\r' then (open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
       moveto 0 (x_size/2); set_color black;
       draw_string "Info submitted. Waiting...";)
       else
         (lobbyname := !lobbyname^Char.escaped event.key;print_string !lobbyname;
          open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
          moveto 0 (x_size/2); set_color black;
          draw_string "Enter lobbyname. Press enter to send.";
          moveto 0 (x_size/3); draw_string !lobbyname;
          lob_interactive ()); in
let start= lob_paint; lob_interactive (); in ();
         )
   |15-> if List.length !next > 0 then (prev:= (!prev @ !current);
     current:= first_n lobby_size !next;
     next:= (List.rev (first_n (List.length !next - List.length !current) (List. rev !next)));
     draw_grid x_size y_size);
   |_-> ());
  print_endline "";
  mouse_pos () in ();




let paint= open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
  moveto 0 (x_size/2); set_color black;
  draw_string "Enter username. Press enter to send.";
  moveto 0 (x_size/3); draw_string !username; in

let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  print_char event.key; print_newline ();
  if event.key == '\b' then ((if String.length (!username) > 0
                              then username := (String.sub (!username) 0 ((String.length !username) -1))); (print_string !username;
                                                                                                               open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
                                                                                                               moveto 0 (x_size/2); set_color black;
                                                                                                               draw_string "Enter username. Press enter to send.";
                                                                                                               moveto 0 (x_size/3); draw_string !username;
                                                                                                               interactive ());)
  else if event.key == '\r' then (open_graph (" "^(string_of_int x_size)^"x"^(string_of_int y_size));
            draw_grid x_size y_size; mouse_pos ();)
  else
  (username := !username^Char.escaped event.key;print_string !username;
  open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
   moveto 0 (x_size/2); set_color black;
  draw_string "Enter username. Press enter to send.";
   moveto 0 (x_size/3); draw_string !username;
   interactive ()); in ();







let start= paint; interactive () in ();