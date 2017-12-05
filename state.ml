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

let step = failwith "Unimplemented"
let add_player = failwith "Unimplemented"
let remove_player = failwith "Unimplemented"
