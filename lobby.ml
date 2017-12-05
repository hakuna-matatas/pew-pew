open GMain
open GdkKeysyms
open GToolbox
open GdkEvent

let locale = GtkMain.Main.init ()

let try_user= input_string "Username" ""
let get_username= match try_user with
  | None-> "Bob"
  | Some a-> (match a with
      | ""-> "No name"
      | a-> a)

let main () =

  let window = GWindow.window ~width:300 ~height:300
      ~title:"Apex" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let username= get_username in
  let make a =
  (* Button *)
    (GButton.button ~label:(string_of_int a)
       (*fun () shoudl make a router call to join a lobby with id [a]*)
       ~packing:vbox#add ())#connect#clicked ~callback: (fun () -> prerr_endline (("Join lobby ")^(string_of_int a)^username)); in
  (* lobby list is list of currently open lobbies*)
  let lobby_list = [24; 34; 45] in
  List.map make lobby_list;
  let openbutton = GButton.button ~label:"Create new lobby."
                              ~packing:vbox#add () in
  openbutton#connect#clicked ~callback: (fun () -> prerr_endline ("Create new lobby."^username));

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
