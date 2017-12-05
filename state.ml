include Type

type t = {
  size            : pos;
  mutable s_rad   : rad;
  s_id            : id;
  map             : Collision.t;
  mutable time    : int;
  mutable ammo    : ammo list;
  mutable bullets : bullet list;
  mutable rocks   : rock list;
  guns            : (id, gun) Hashtbl.t;
  players         : (id, player) Hashtbl.t;
}

let map_hash f h =
  let f' id e acc = e :: acc in
  Hashtbl.fold f' h [] |> List.map f

let to_json_string s =
  let x, y    = s.size in
  let a = `List (List.map ammo_to_json s.ammo) in
  let b = `List (List.map bullet_to_json s.bullets) in
  let r = `List (List.map rock_to_json s.rocks) in
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
  let a = List.map ammo_to_entity s.ammo in
  let b = List.filter (fun b -> b.b_own = "") s.bullets in
  let b' = List.map bullet_to_entity b in
  let r = List.map rock_to_entity s.rocks in
  let g = map_hash gun_to_entity s.guns in
  let p = map_hash player_to_entity s.players in
  a @ b' @ r @ g @ p

type collision =
| Bounce
| Hit of int
| Ammo of id
| Equip of id
| Destroy of (id * id)

let handle_player_ammo p_id a_id = failwith "Unimplemented"
let handle_player_gun p_id g_id = failwith "Unimplemented"
let handle_player_bullet p_id b_id = failwith "Unimplemented"

let handle_collision e e' = match e, e' with
| Ammo   (a_id, _, _), Player (p_id, _, _) 
| Player (p_id, _, _), Ammo   (a_id, _, _) -> handle_player_ammo p_id a_id
| Bullet (b_id, _, _), Player (p_id, _, _) 
| Player (p_id, _, _), Bullet (b_id, _, _) -> handle_player_bullet p_id b_id
| Gun    (g_id, _, _), Player (p_id, _, _)
| Player (p_id, _, _), Gun    (g_id, _, _) -> handle_player_gun p_id g_id
| _ -> failwith "Unimplemented"

let step_bullet s b = List.map (fun b' -> b'.b_step b') b

let step s = 
  let _ = s.bullets <- List.map (fun b -> b.b_step b) s.bullets in
  let _ = List.map bullet_to_entity s.bullets |> List.iter Collision.update in
  let _ = s.time <- s.time + 1 in 
  Collision.all s.map |> List.iter handle_collision

let add_player = failwith "Unimplemented"
let remove_player = failwith "Unimplemented"
