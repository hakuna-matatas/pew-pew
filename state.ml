include Type
include Settings

type t = {
  mutable s_rad : rad;
  mutable time  : int;
  size          : pos;
  s_id          : id;
  gen           : Generate.t;
  map           : Collision.t;
  ammo          : (id, ammo)   Hashtbl.t;
  bullets       : (id, bullet) Hashtbl.t;
  rocks         : (id, rock)   Hashtbl.t;
  guns          : (id, gun)    Hashtbl.t;
  players       : (id, player) Hashtbl.t;
}

(* Converts Hashtbl to list of values. *)
let to_values h = 
  let f' id e acc = e :: acc in
  Hashtbl.fold f' h []

(* Equivalent to their list counterparts *)
let iter_hash f h = to_values h |> List.iter f
let map_hash f h = to_values h |> List.map f
let filter_hash f h = to_values h |> List.filter f

let to_json_string s =
  let x, y = s.size in
  let a = `List (map_hash ammo_to_json s.ammo) in
  let b = `List (map_hash bullet_to_json s.bullets) in
  let r = `List (map_hash rock_to_json s.rocks) in
  let g = `List (map_hash gun_to_json s.guns) in
  let p = `List (map_hash player_to_json s.players) in
  Yojson.Basic.to_string (`Assoc [
    ("size"    , `List [`Float x; `Float y]);
    ("rad"     , `Float s.s_rad);
    ("ammo"    , a);
    ("bullets" , b);
    ("guns"    , g);
    ("players" , p);
    ("rocks"   , r);
  ])

let to_list s = 
  let a  = map_hash ammo_to_entity s.ammo in
  let b  = filter_hash (fun b -> b.b_own = "") s.bullets in
  let b' = List.map bullet_to_entity b in
  let r  = map_hash rock_to_entity s.rocks in
  let g  = map_hash gun_to_entity s.guns in
  let p  = map_hash player_to_entity s.players in
  a @ b' @ r @ g @ p

let rec repeat f n = 
  if n <= 0 then () 
  else let _ = f () in repeat f (n - 1)

let free s = Collision.free s.map s.size

let create_ammo s () =
  let g = map_hash (fun g -> g.g_type) s.guns in
  let a = Generate.ammo s.gen (free s) g in
  let e = ammo_to_entity a in
  let _ = Collision.update s.map e in
  Hashtbl.add s.ammo a.a_id a

let create_gun s () =
  let g = Generate.gun s.gen (free s) in
  let e = gun_to_entity g in
  let _ = Collision.update s.map e in
  Hashtbl.add s.guns g.g_id g

let create_rock s () = 
  let r = Generate.rock s.gen (free s) in
  let e = rock_to_entity r in
  let _ = Collision.update s.map e in
  Hashtbl.add s.rocks r.r_id r

let create_player s id =
  let p = Generate.player s.gen (free s) id in
  let e = player_to_entity p in
  let _ = Collision.update s.map e in
  Hashtbl.add s.players p.p_id p

let create id = 
  let s = {
    s_id    = id;     
    s_rad   = ring_radius;
    size    = map_width, map_height;
    time    = 0;
    gen     = Generate.create ();
    map     = Collision.create ();
    ammo    = Hashtbl.create 50;
    bullets = Hashtbl.create 50;
    guns    = Hashtbl.create 10;
    players = Hashtbl.create 4;
    rocks   = Hashtbl.create 50;
  } in
  repeat (create_rock s) initial_rocks;
  repeat (create_gun  s) initial_guns;
  repeat (create_ammo s) initial_ammo;
  s

let has_type s inv t = List.find_opt (fun g_id -> let g = Hashtbl.find s.guns g_id in g.g_type = t) inv

(* Player-ammo interaction. If player owns the correct
 * gun type for the ammo drop, pick it up; otherwise collision occurs. *)
let collision_pa s p_id a_id = 
  let p = Hashtbl.find s.players p_id in
  let a = Hashtbl.find s.ammo    a_id in
  match has_type s p.p_inv a.a_gun with
  | None    -> true
  | Some g_id  -> let g = Hashtbl.find s.guns g_id in
    let g' = {g with g_ammo = g.g_ammo + a.a_amt} in
    let _  = Hashtbl.replace  s.guns g'.g_id g' in
    let _  = Hashtbl.remove   s.ammo a_id       in
    let _  = Collision.remove s.map (ammo_to_entity a) in false

(* Player-bullet interaction. Player always
 * takes damage, with friendly fire. *)
let collision_pb s p_id b_id =
  let p  = Hashtbl.find s.players p_id in
  let b  = Hashtbl.find s.bullets b_id in
  let hp' = p.p_hp - b.b_dmg in
  let p' = {p with p_hp = hp'} in
  let _  = Hashtbl.replace  s.players p_id p' in
  let _  = Hashtbl.remove   s.bullets b_id    in
  let _  = Collision.remove s.map (bullet_to_entity b) in
  let _  = if hp' <= 0 then Collision.remove s.map (player_to_entity p)
  else () in false

(* Player-gun interaction. If player doesn't own this
 * type of gun, pick it up; otherwise collide. *)
let collision_pg s p_id g_id =
  let p = Hashtbl.find s.players p_id in
  let g = Hashtbl.find s.guns    g_id in
  match has_type s p.p_inv g.g_type with
  | Some _ -> true
  | None   ->
    let g' = {g with g_own = p_id} in
    let _  = Hashtbl.replace s.guns g.g_id g' in
    Collision.remove s.map (gun_to_entity g); false

(* Deletes bullet with id [b_id] from state [s]. *)
let destroy_bullet s b_id = 
  let b = Hashtbl.find s.bullets b_id in
  let _ = Hashtbl.remove s.bullets b_id in
  let _ = Collision.remove s.map (bullet_to_entity b) in ()

(* Deletes ammo with id [a_id] from state [s]. *)
let destroy_ammo s a_id =
  let a = Hashtbl.find s.ammo    a_id in
  let _ = Hashtbl.remove s.ammo  a_id in
  let _ = Collision.remove s.map (ammo_to_entity a) in ()

(* Deletes gun with id [g_id] from state [s]. *)
let destroy_gun s g_id =
  let g = Hashtbl.find s.guns    g_id in
  let _ = Hashtbl.remove s.guns  g_id in
  let _ = Collision.remove s.map (gun_to_entity g) in
  match g.g_own with
  | ""   -> ()
  | p_id -> let p = Hashtbl.find s.players p_id in
    let inv' = List.filter (fun id -> id <> g.g_id) p.p_inv in
    Hashtbl.replace s.players p_id {p with p_inv = inv'}

(* Deletes player with id [p_id] from state [s], along with all owned guns. *)
let destroy_player s p_id =
  let p = Hashtbl.find s.players p_id in
  let _ = Hashtbl.remove s.players p_id in
  let _ = Collision.remove s.map (player_to_entity p) in
  let _ = List.iter (fun g_id -> Hashtbl.remove s.guns g_id) p.p_inv in ()

(* Imposes order on collision pairs for cleaner pattern matching. *)
let order e e' = match e, e' with
| Bullet _, Player _
| Ammo   _, Player _
| Gun    _, Player _
| Rock   _, Player _
| Ammo   _, Bullet _
| Gun    _, Bullet _
| Rock   _, Bullet _ -> (e', e)
| _, _ -> (e, e')

(* Handles collision between entities.
 *
 * returns true if movement was impeded (i.e. move was invalid)
 * and false otherwise.
 *)
let collision s (e, e') = match order e e' with
| Player (p_id, _, _), Ammo   (a_id, _, _)  -> collision_pa s p_id a_id
| Player (p_id, _, _), Bullet (b_id, _, _)  -> collision_pb s p_id b_id
| Player (p_id, _, _), Gun    (g_id, _, _)  -> collision_pg s p_id g_id
| Player (p_id, _, _), Rock   (r_id, _, _)  -> true
| Player (p_id, _, _), Player (p_id', _, _) -> true
| Bullet (b_id, _, _), Ammo   (a_id, _, _)  -> destroy_bullet s b_id; destroy_ammo s a_id; false
| Bullet (b_id, _, _), Bullet (b_id', _, _) -> destroy_bullet s b_id; destroy_bullet s b_id'; false
| Bullet (b_id, _, _), Gun    (g_id, _, _)  -> destroy_bullet s b_id; destroy_gun s g_id; false
| Bullet (b_id, _, _), Rock   (r_id, _, _)  -> destroy_bullet s b_id; false
| _, _ -> failwith "Error in map generation"  

let outside s p =
  let x1, y1 = p.p_pos in
  let x2, y2 = s.size in
  let x = x2 -. (x1 /. 2.0) in
  let y = y2 -. (y1 /. 2.0) in
  s.s_rad *. s.s_rad < (x*.x +. y*.y)

let sqdist (x1, y1) (x2, y2) =
  let x = x2 -. x1 in
  let y = y2 -. y1 in
  (x*.x) +. (y*.y)

(* Stepping implementation must do the following: 
 *
 * 1) Update position of all bullets
 * 2) Handle collisions caused by bullet changes
 * 3) Update all gun cooldowns
 * 4) Increase time step 
 * 5) Decrease radius
 * 6) Check for out-of-bound players
 * 7) Remove dead players
 * 8) Spawn new ammo/guns
 *
 *)
let step s = 
  let _ = iter_hash (fun b -> Hashtbl.replace s.bullets b.b_id (b.b_step b)) s.bullets in
  let _ = map_hash bullet_to_entity s.bullets |> List.iter (Collision.update s.map) in
  let _ = Collision.all s.map |> List.map (collision s) in
  let _ = iter_hash (fun g -> Hashtbl.replace s.guns g.g_id {g with g_cd = max 0 (g.g_cd - gun_cd_rate)}) s.guns in
  let _ = s.time <- s.time + 1 in 
  let _ = s.s_rad <- s.s_rad -. constrict_rate in
  let _ = iter_hash (fun p -> if outside s p then Hashtbl.replace s.players p.p_id {p with p_hp = 0}) s.players in
  let _ = iter_hash (fun p -> if p.p_hp <= 0 then destroy_player s p.p_id else ()) s.players in
  let _ = if (s.time mod ammo_spawn_cd) = 0 then repeat (create_ammo s) ammo_spawn_count else () in
  let _ = if (s.time mod gun_spawn_cd)  = 0 then repeat (create_gun s)  gun_spawn_count  else () in
  ()

let fire s p_id g_id =
  let p = Hashtbl.find s.players p_id in
  let g = Hashtbl.find s.guns    g_id in
  if p.p_hp <= 0 then destroy_player s p.p_id else
  if not (List.exists (fun id -> id = g_id) p.p_inv) then () else
  if not (g.g_own = p.p_id) then () else
  let bullets = Generate.bullet s.gen p g in
  List.iter (fun b -> 
      let _ = Hashtbl.add s.bullets b.b_id b in
      Collision.update s.map (bullet_to_entity b)
    ) bullets

let move s p_id pos =
  let p = Hashtbl.find s.players p_id in
  if p.p_hp <= 0 then destroy_player s p.p_id else
  if sqdist p.p_pos pos > max_sq_distance then () else
  let p' = {p with p_pos = pos} in
  let _ = Collision.update s.map (player_to_entity p') in
  Hashtbl.replace s.players p_id p'
