open Ctype
open Settings

let p_name = ref ""
let p_id   = ref (-1)
let g_id   = ref (-1)

let string_of_players p = let s = p
  |> List.fold_left (fun acc p -> acc ^ p ^ ", ") "" in
  String.sub s 0 (String.length s - 2)

let print_description d =
  print_newline ();
  print_endline ("Name   : " ^ d.game_name);
  print_endline ("ID     : " ^ (string_of_int d.game_id));
  print_endline ("Players: " ^ string_of_players d.game_players);
  print_newline ()

let print_help () =
  print_newline ();
  print_endline ("Welcome to the Apex lobby!");
  print_endline ("Type 'games' to view a list of all available games.");
  print_endline ("Type 'create <GAME_NAME>' to create a new game with name <GAME_NAME>.");
  print_endline ("Type 'join <GAME_ID>' to join the game with ID <GAME_ID>.");
  print_endline ("Type 'help' to bring up this information again.");
  print_newline ()

let prompt () =
  print_newline ();
  print_endline ("Welcome to Apex! Please enter a username below.");
  let s = read_line () in
  print_newline (); s

let lobbies () = Router.get_lobbies (fun l -> l)

type command =
| List
| Join of id
| Create of name

let parse s = match Str.split (Str.regexp " ") s with
| "games"  :: []         -> Some List
| "create" :: name :: [] -> Some (Create name)
| "join"   :: id   :: [] -> 
  begin match int_of_string_opt id with
  | Some n -> Some (Join n)
  | None   -> None end
| _ -> None

let eval = function
| List   -> List.iter print_description (lobbies ())
| Create name -> 
  begin
    let _ = Router.create_lobby name !p_name 
    (fun r -> p_id := r.rplayer_id; g_id := r.rgame_id) in ()
  end
| Join n -> 
  begin 
    if List.exists (fun l -> l.game_id = n) (lobbies ()) then
      let _ = Router.join_lobby n !p_name (fun r -> p_id := r; g_id := n) in ()
    else ()
  end

let rec lobby () = match read_line () |> parse with
| None -> print_newline (); print_endline "Invalid command."; print_newline (); lobby ()
| Some c -> let _ = eval c in
  if (!p_id <> (-1)) && (!g_id <> (-1)) then 
    let _ = Client.create !g_id !p_id |> Client.run in ()
  else
    lobby ()
  
let rec init () =
  if !p_name = "" then let _ = p_name := prompt () in init ()
  else print_help (); lobby ()

let () = init ()
