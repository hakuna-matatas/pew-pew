include Type

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

(* Maps [f] over all key-value pairs in Hashtbl [h]. *)
let map_hash f h =
  let f' id e acc = e :: acc in
  Hashtbl.fold f' h [] |> List.map f

(* Filters all key-value pairs in Hashtbl [h] through function [f]. *)
let filter_hash f h = 
  let f' id e acc = if f e then e :: acc else acc in
  Hashtbl.fold f' h []

let to_json_string s =
  let x, y    = s.size in
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
let delete_bullet s b_id = 
  let b = Hashtbl.find s.bullets b_id in
  let _ = Hashtbl.remove s.bullets b_id in
  let _ = Collision.remove s.map (bullet_to_entity b) in ()

(* Bullet-ammo interaction. Destroy both. *)
let collision_ba s b_id a_id = 
  let a = Hashtbl.find s.ammo    a_id in
  let _ = Hashtbl.remove s.ammo  a_id in
  let _ = Collision.remove s.map (ammo_to_entity a) in
  delete_bullet s b_id; false

(* Bullet-bullet interaction. Destroy both. *)
let collision_bb s b_id b_id' =
  delete_bullet s b_id; delete_bullet s b_id'; false

(* Bullet-gun interaction. Destroy both. *)
let collision_bg s b_id g_id =
  let g = Hashtbl.find s.guns    g_id in
  let _ = Hashtbl.remove s.guns  g_id in
  let _ = Collision.remove s.map (gun_to_entity g) in
  delete_bullet s b_id; false

(* Bullet-rock interaction. Destroy bullet. *)
let collision_br s b_id r_id = delete_bullet s b_id; false

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
| Bullet (b_id, _, _), Ammo   (a_id, _, _)  -> collision_ba s b_id a_id
| Bullet (b_id, _, _), Bullet (b_id', _, _) -> collision_bb s b_id b_id'
| Bullet (b_id, _, _), Gun    (g_id, _, _)  -> collision_bg s b_id g_id
| Bullet (b_id, _, _), Rock   (r_id, _, _)  -> collision_br s b_id r_id
| _, _ -> failwith "Error in map generation"  

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

let create id = 
  let scale = Type.map_scale () in
  let s = {
    s_id    = id;     
    s_rad   = scale.m_scale *. 1.20;
    size    = (scale.m_scale, scale.m_scale);
    time    = 0;
    gen     = Generate.create ();
    map     = Collision.create ();
    ammo    = Hashtbl.create 50;
    bullets = Hashtbl.create 50;
    guns    = Hashtbl.create 10;
    players = Hashtbl.create 4;
    rocks   = Hashtbl.create 50;
  } in
  repeat (create_rock s) 50;
  repeat (create_gun  s) 10;
  repeat (create_ammo s) 50;
  s

(* Stepping implementation must do the following: 
 *
 * 1) Update position of all bullets
 * 2) Handle collisions caused by bullet changes
 * 3) Update all gun cooldowns TODO
 * 4) Increase time step 
 * 5) Decrease radius TODO
 * 6) Check for out-of-bound players TODO
 * 7) Spawn new ammo/guns TODO
 *
 *)
let step s = 
  let _ = map_hash (fun b -> Hashtbl.replace s.bullets b.b_id (b.b_step b)) s.bullets in
  let _ = map_hash bullet_to_entity s.bullets |> List.iter (Collision.update s.map) in
  let _ = s.time <- s.time + 1 in 
  let _ = Collision.all s.map |> List.map (collision s) in ()

let add_player = failwith "Unimplemented"
let remove_player = failwith "Unimplemented"
