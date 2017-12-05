include Type

type t = {
  mutable s_rad : rad;
  mutable time  : int;
  size          : pos;
  s_id          : id;
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

let contains l id = List.exists (fun id' -> id = id') l

let collision_pa s p_id a_id = 
  let p = Hashtbl.find s.players p_id in
  let a = Hashtbl.find s.ammo    a_id in
  let g_id = a.a_gun in
  if contains p.p_inv g_id then
    let g  = Hashtbl.find s.guns g_id in
    let g' = {g with g_ammo = g.g_ammo + a.a_amt} in
    let _  = Hashtbl.replace  s.guns g_id g' in
    let _  = Hashtbl.remove   s.ammo a_id    in
    let _  = Collision.remove s.map (ammo_to_entity a) in false
  else true

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

let collision_pg s p_id g_id =
  let p = Hashtbl.find s.players p_id in
  let g = Hashtbl.find s.guns    g_id in
  if contains p.p_inv g.g_id then true
  else
    let g' = {g with g_own = p_id} in
    let _  = Hashtbl.replace s.guns g.g_id g' in
    Collision.remove s.map (gun_to_entity g); false

let delete_bullet s b_id = 
  let b = Hashtbl.find s.bullets b_id in
  let _ = Hashtbl.remove s.bullets b_id in
  let _ = Collision.remove s.map (bullet_to_entity b) in ()

let collision_ba s b_id a_id = 
  let a = Hashtbl.find s.ammo    a_id in
  let _ = Hashtbl.remove s.ammo    a_id in
  let _ = Collision.remove s.map (ammo_to_entity a) in
  delete_bullet s b_id; false

let collision_bb s b_id b_id' =
  delete_bullet s b_id; delete_bullet s b_id'; false

let collision_bg s b_id g_id =
  let g = Hashtbl.find s.guns    g_id in
  let _ = Hashtbl.remove s.guns    g_id in
  let _ = Collision.remove s.map (gun_to_entity g) in
  delete_bullet s b_id; false

let collision_br s b_id r_id = delete_bullet s b_id; false

let order e e' = match e, e' with
| Bullet _, Player _
| Ammo   _, Player _
| Gun    _, Player _
| Rock   _, Player _
| Ammo   _, Bullet _
| Gun    _, Bullet _
| Rock   _, Bullet _ -> (e', e)
| _, _ -> (e, e')

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

let step_bullet s b = List.map (fun b' -> b'.b_step b') b

let step s = 
  let _ = map_hash (fun b -> Hashtbl.replace s.bullets b.b_id (b.b_step b)) s.bullets in
  let _ = map_hash bullet_to_entity s.bullets |> List.iter (Collision.update s.map) in
  let _ = s.time <- s.time + 1 in 
  let _ = Collision.all s.map |> List.map (collision s) in ()

let add_player = failwith "Unimplemented"
let remove_player = failwith "Unimplemented"
