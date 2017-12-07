open Graphics
open Settings
open Ctype
open Sdlevent
open Sdlkey
include Draw

type t = {
  clp_id      : id;
  clg_id      : id;
  lock        : Mutex.t;
  mutable g   : name;
  mutable sel : int;
  mutable pos : pos;
  mutable dir : dir;
  mutable state : state;
}

let create clg_id clp_id = 
  Router.get_world_state clg_id clp_id (fun s -> {
      clp_id; clg_id; pos = (0, 0); sel = 0; g = ""; dir = N; state = s; lock = Mutex.create ();
  })

let get_direction () =
  if is_key_pressed KEY_w && is_key_pressed KEY_a then Some NW else
  if is_key_pressed KEY_w && is_key_pressed KEY_d then Some NE else
  if is_key_pressed KEY_s && is_key_pressed KEY_d then Some SE else
  if is_key_pressed KEY_s && is_key_pressed KEY_a then Some SW else
  if is_key_pressed KEY_w then Some N else
  if is_key_pressed KEY_d then Some E else
  if is_key_pressed KEY_s then Some S else
  if is_key_pressed KEY_a then Some W else None

let move st d =
  let (x, y) = st.pos in
  let cps  = client_player_speed in
  let cps' = int_of_float (sqrt 2. *. (float_of_int cps) /. 2.) in
  let _ = Mutex.lock st.lock in
  let _ = match d with
  | NW -> st.pos <- (x - cps' , y + cps')
  | NE -> st.pos <- (x + cps' , y + cps')
  | SE -> st.pos <- (x + cps' , y - cps')
  | SW -> st.pos <- (x - cps' , y - cps')
  | N  -> st.pos <- (x , y + cps)
  | E  -> st.pos <- (x + cps , y)
  | S  -> st.pos <- (x , y - cps)
  | W  -> st.pos <- (x - cps , y) in
  let _ = Mutex.unlock st.lock in
  st.dir <- d

let rec local st () =
  Sdltimer.delay client_tick_cooldown;

  let _ = match get_direction () with
  | None   -> ()
  | Some d -> move st d in

  let _ = if is_key_pressed KEY_q then st.sel <- st.sel - 1 else () in
  let _ = if is_key_pressed KEY_e then st.sel <- st.sel + 1 else () in

  let _ = Mutex.lock st.lock in
  let _ = draw_state st.state st.clp_id st.g st.dir in
  let _ = Mutex.unlock st.lock in
  local st ()

let rec loop st () =
  Sdltimer.delay 5;

  let _ = Router.move_location st.clg_id st.clp_id st.pos (fun s -> s) in
  let st' = Router.get_world_state st.clg_id st.clp_id (fun s -> s) in
  let p' = List.find (fun p -> p.p_id = st.clp_id) st'.players in

  if List.length p'.p_inv = 0 then 
    let _ = Mutex.lock st.lock in
      st.pos <- p'.p_pos; st.state <- st';
    let _ = Mutex.unlock st.lock in
    loop st ()
  else
  let g_id = List.nth p'.p_inv (st.sel mod (List.length p'.p_inv)) in
  let g = List.find (fun g -> g.g_id = g_id) st'.guns in
  let _ = if is_key_pressed KEY_SPACE then
    Router.fire st.clg_id st.clp_id g_id (fun s -> ())
  else () in
  Mutex.lock st.lock;
  st.g <- g.g_type; st.pos <- p'.p_pos; st.state <- st';
  Mutex.unlock st.lock;
  loop st ()

let run st =
  open_graph (" " ^ (string_of_int client_width) ^ "x" ^ (string_of_int client_height));
  set_window_title "Apex";

  Sdl.init [`EVENTTHREAD; `VIDEO; `TIMER];
  Sdlkey.enable_key_repeat ?delay:(Some 0) ?interval:(Some 10) ();
  Sdlvideo.set_video_mode 1 1 [];

  let _ = Thread.create (loop st) () in
  let _ = Thread.create (local st) () in ()

