open Graphics
open Array
open Router

type description = {
  game_name    : string;
  game_id      : int;
  game_players : string list
}

let x_size= 500;;

let y_size= 500;;

let lobby_size= 12;;

let lobby = [{game_name="a";game_id=1;game_players=["Bob";"Bill";"Billy"]};
             {game_name="baa";game_id=2;game_players=["Woof";"Moo";"Meow"]};
             {game_name="choo";game_id=3;game_players=["Beep";"Bop";"Whop"]};
             {game_name="dog";game_id=4;game_players=["Duffy";"Doofy"]};
             {game_name="egg";game_id=5;game_players=["ugh"]};
             {game_name="rope";game_id=6;game_players=["h";"e";"l";"l";"o"]};
             {game_name="doggie";game_id=6;game_players=[" ";"1";"2456"]};
             {game_name="a";game_id=1;game_players=["Bob";"Bill";"Billy"]};
             {game_name="baa";game_id=2;game_players=["Woof";"Moo";"Meow"]};
             {game_name="choo";game_id=3;game_players=["Beep";"Bop";"Whop"]};
             {game_name="dog";game_id=4;game_players=["Duffy";"Doofy"]};
             {game_name="egg";game_id=5;game_players=["ugh"]};
             {game_name="rope";game_id=6;game_players=["h";"e";"l";"l";"o"]};
             {game_name="doggie";game_id=6;game_players=[" ";"1";"2456"]};
             {game_name="a";game_id=1;game_players=["Bob";"Bill";"Billy"]};
             {game_name="baa";game_id=2;game_players=["Woof";"Moo";"Meow"]};
             {game_name="choo";game_id=3;game_players=["Beep";"Bop";"Whop"]};
             {game_name="dog";game_id=4;game_players=["Duffy";"Doofy"]};
             {game_name="egg";game_id=5;game_players=["ugh"]};
             {game_name="rope";game_id=6;game_players=["h";"e";"l";"l";"o"]};
             {game_name="doggie";game_id=6;game_players=[" ";"1";"2456"]};
             {game_name="a";game_id=1;game_players=["Bob";"Bill";"Billy"]};
             {game_name="baa";game_id=2;game_players=["Woof";"Moo";"Meow"]};
             {game_name="choo";game_id=3;game_players=["Beep";"Bop";"Whop"]};
             {game_name="dog";game_id=4;game_players=["Duffy";"Doofy"]};
             {game_name="egg";game_id=5;game_players=["ugh"]};
             {game_name="rope";game_id=6;game_players=["h";"e";"l";"l";"o"]};
             {game_name="doggie";game_id=6;game_players=[" ";"1";"2456"]};
            ];;


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

  let rec display_player x y num lst= moveto x (y-20-(10*num));
    match lst with
    |[]-> ()
    |h::t-> draw_string ("-"^h); display_player x y (num+1) t in

  if !counter< 0 then counter:=0;

  (if !counter <> 0 then
     moveto 15 (y_size*19/20);
   draw_string (get ids 0).game_name;
   display_player 15 (y_size*19/20) 0 (get ids 0).game_players;
   decr counter);

  (if !counter <> 0 then
     moveto (x_size/4+15) (y_size*19/20);
   draw_string (get ids 1).game_name;
   display_player (x_size/4+15) (y_size*19/20) 0 (get ids 1).game_players;
   decr counter);

  (if !counter <> 0 then
     (moveto (x_size/2+15) (y_size*19/20);
      draw_string (get ids 2).game_name;
      display_player (x_size/2+15) (y_size*19/20) 0 (get ids 2).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size*3/4+15) (y_size*19/20);
      draw_string (get ids 3).game_name;
      display_player (x_size*3/4+15) (y_size*19/20) 0 (get ids 3).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (15) (y_size*14/20);
      draw_string (get ids 4).game_name;
      display_player (15) (y_size*14/20) 0 (get ids 4).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size/4+15) (y_size*14/20);
      draw_string (get ids 5).game_name;
      display_player (x_size/4+15) (y_size*14/20) 0 (get ids 5).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size/2+15) (y_size*14/20);
      draw_string (get ids 6).game_name;
      display_player (x_size/2+15) (y_size*14/20) 0 (get ids 6).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size*3/4+15) (y_size*14/20);
      draw_string (get ids 7).game_name;
      display_player (x_size*3/4+15) (y_size*14/20) 0 (get ids 7).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (15) (y_size*9/20);
      draw_string (get ids 8).game_name;
      display_player (15) (y_size*9/20) 0 (get ids 8).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size/4+15) (y_size*9/20);
      draw_string (get ids 9).game_name;
      display_player (x_size/4+15) (y_size*9/20) 0 (get ids 9).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size/2+15) (y_size*9/20);
      draw_string (get ids 10).game_name;
      display_player (x_size/2+15) (y_size*9/20) 0 (get ids 10).game_players;
      decr counter);

   if !counter <> 0 then
     (moveto (x_size*3/4+15) (y_size*9/20);
      draw_string (get ids 11).game_name;
      display_player (x_size*3/4+15) (y_size*9/20) 0 (get ids 11).game_players;
      decr counter);

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
   |1-> if (List.length (!current) > 0)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |2-> if (List.length (!current) > 1)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |3-> if (List.length (!current) > 2)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |4-> if (List.length (!current) > 3)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |5-> if (List.length (!current) > 4)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |6-> if (List.length (!current) > 5)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |7-> if (List.length (!current) > 6)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |8-> if (List.length (!current) > 7)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |9-> if (List.length (!current) > 8)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |10-> if (List.length (!current) > 9)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |11-> if (List.length (!current) > 10)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |12-> if (List.length (!current) > 11)
     then (join_lobby ((get ids (grid_num-1)).game_id) (!username) (fun x-> ());

   |13-> if List.length !prev > 0
     then (next:= (!current @ !next);
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

            if event.key == '\b'
            then ((if String.length (!lobbyname) > 0
                   then lobbyname := (String.sub (!lobbyname) 0 ((String.length !lobbyname) -1)));
                  (print_string !lobbyname;
                   open_graph " 500x500"; set_color white;
                   fill_rect 0 0 x_size y_size;
                   moveto 0 (x_size/2); set_color black;
                   draw_string "Enter lobbyname. Press enter to send.";
                   moveto 0 (x_size/3); draw_string !lobbyname;
                   lob_interactive ());)

            else if event.key == '\r'
            (* CREATE NEW LOBBY *)
            then (open_graph " 500x500";
                  create_lobby (!username) (!lobbyname) (fun x-> ()));
                  set_color white; fill_rect 0 0 x_size y_size;
                  moveto 0 (x_size/2); set_color black;
                  draw_string "Info submitted. Waiting...";)

            else
              (lobbyname := !lobbyname^Char.escaped event.key;
               print_string !lobbyname;
               open_graph " 500x500"; set_color white;
               fill_rect 0 0 x_size y_size;
               moveto 0 (x_size/2); set_color black;
               draw_string "Enter lobbyname. Press enter to send.";
               moveto 0 (x_size/3); draw_string !lobbyname;
               lob_interactive ()); in

          let start= lob_paint; lob_interactive (); in ();

         )
   |15-> if List.length !next > 0

     then (prev:= (!prev @ !current);
           current:= first_n lobby_size !next;
           next:= (List.rev (first_n (List.length !next - List.length !current) (List. rev !next)));
           draw_grid x_size y_size);

   |_-> ());

  print_endline "";
  mouse_pos () in ();


let paint= open_graph " 500x500"; set_color white;
  fill_rect 0 0 x_size y_size;
  moveto 0 (x_size/2); set_color black;
  draw_string "Enter username. Press enter to send.";
  moveto 0 (x_size/3); draw_string !username; in

let rec interactive () =
  let event = wait_next_event [Key_pressed] in
  print_char event.key; print_newline ();
  if event.key == '\b'

  then ((if String.length (!username) > 0
         then username := (String.sub (!username) 0 ((String.length !username) -1)));
        (print_string !username;
         open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
         moveto 0 (x_size/2); set_color black;
         draw_string "Enter username. Press enter to send.";
         moveto 0 (x_size/3); draw_string !username;
         interactive ());)

  else if event.key == '\r'

  then (open_graph (" "^(string_of_int x_size)^"x"^(string_of_int y_size));
        draw_grid x_size y_size; mouse_pos ();)

  else
    (username := !username^Char.escaped event.key;print_string !username;
     open_graph " 500x500"; set_color white; fill_rect 0 0 x_size y_size;
     moveto 0 (x_size/2); set_color black;
     draw_string "Enter username. Press enter to send.";
     moveto 0 (x_size/3); draw_string !username;
     interactive ()); in ();



let start= paint; interactive () in ();
