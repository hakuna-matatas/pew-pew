include Type

type t = {
  size            : pos;
  mutable s_rad   : rad;
  s_id            : id;
  map             : Collision.t;
  mutable time    : int;
  mutable ammo    : ammo list;
  mutable bullets : bullet list;
  guns            : (id, gun) Hashtbl.t;
  rocks           : (id, rock) Hashtbl.t;
  players         : (id, player) Hashtbl.t;
}

let map_hash h f =
  let f' id e acc = e :: acc in
  Hashtbl.fold f' h [] |> List.map f

let to_json_string s =
  let x, y    = s.size in
  let ammo    = `List (List.map ammo_to_json s.ammo) in
  let bullets = `List (List.map bullet_to_json s.bullets) in
  let guns    = `List (map_hash s.guns gun_to_json) in
  let players = `List (map_hash s.players player_to_json) in
  let rocks   = `List (map_hash s.rocks rock_to_json) in
  let s' = `Assoc [
    ("size"    , `List [`Float x; `Float y]);
    ("rad"     , `Float s.s_rad);
    ("ammo"    , ammo);
    ("bullets" , bullets);
    ("guns"    , guns);
    ("players" , players);
    ("rocks"   , rocks);
  ] in s' |> Yojson.Basic.to_string

let step = failwith "Unimplemented"
let add_player = failwith "Unimplemented"
let remove_player = failwith "Unimplemented"
let to_list = failwith "Unimplemented"
