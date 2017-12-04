type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW

type g_id = string
type p_id = string
type r_id = int
type s_id = string

type ammo = {
  a_gun : g_id; 
  a_pos : pos;
  a_rad : rad;
  a_amt : int;
}

and bullet = {
  b_gun  : g_id;
  b_own  : p_id;
  b_pos  : pos;
  b_rad  : rad;
  b_dmg  : int;
  b_step : bullet -> state -> bullet;
}

and gun = {
  g_id   : g_id;
  g_cd   : int;
  g_own  : p_id;
  g_pos  : pos;
  g_rad  : rad;
  g_rate : int;
  g_ammo : int;
  g_fire : player -> bullet list;
}

and player = {
  p_id  : p_id;
  p_hp  : int;
  p_pos : pos;
  p_rad : rad;
  p_dir : dir;
  p_inv : g_id list;
}

and rock = {
  r_id  : r_id;
  r_pos : pos;
  r_rad : rad;
}

and entity = 
| Rock   of (r_id * rad * pos) 
| Bullet of (g_id * rad * pos)
| Ammo   of (g_id * rad * pos)
| Gun    of (g_id * rad * pos)
| Player of (p_id * rad * pos)

and map = (entity list) array array

and state = {
  size    : pos;
  s_rad   : rad;
  s_id    : s_id;
  map     : map;
  time    : int;
  ammo    : ammo list;
  bullets : bullet list;
  guns    : (g_id, gun   ) Hashtbl.t;
  rocks   : (r_id, rock  ) Hashtbl.t;
  players : (p_id, player) Hashtbl.t;
}

let dir_to_json = function
| N -> "N" | NE -> "NE" | E -> "E" | SE -> "SE"
| S -> "S" | SW -> "SW" | W -> "W" | NW -> "NW"

let ammo_to_json a = 
  let x, y = a.a_pos in
  `Assoc [
    ("gun"    , `String a.a_gun);
    ("amount" , `Int    a.a_amt);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad"    , `Float  a.a_rad)
  ]

let bullet_to_json b =
  let x, y = b.b_pos in
  `Assoc [
    ("gun" , `String b.b_gun);
    ("rad" , `Float  b.b_rad);
    ("pos" , `List   [`Float x; `Float y])
  ]

let gun_to_json g_id g acc =
  let x, y = g.g_pos in
  let g' = `Assoc [
    ("id"     , `String g.g_id);
    ("owner"  , `String g.g_own);
    ("ready"  , `Bool   (g.g_cd = 0));
    ("ammo"   , `Int    g.g_ammo);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad" , `Float  g.g_rad)
  ] in
  g' :: acc
  
let player_to_json p_id p acc =
  let x, y = p.p_pos in
  let inv' = List.map (fun g_id -> `String g_id) p.p_inv in
  let p'   = `Assoc [
    ("id"  , `String p.p_id);
    ("hp"  , `Int    p.p_hp);
    ("dir" , `String (dir_to_json p.p_dir));
    ("inv" , `List   inv');
    ("pos" , `List   [`Float x; `Float y]);
    ("rad" , `Float  p.p_rad)
  ] in
  p' :: acc

let rock_to_json r_id r acc =
  let x, y = r.r_pos in
  let r'   = `Assoc [
    ("pos" , `List  [`Float x; `Float y]);
    ("rad" , `Float r.r_rad)
  ] in
  r' :: acc

let to_json_string s =
  let x, y    = s.size in
  let ammo    = `List (List.map ammo_to_json s.ammo) in
  let bullets = `List (List.map bullet_to_json s.bullets) in
  let guns    = `List (Hashtbl.fold gun_to_json s.guns []) in
  let players = `List (Hashtbl.fold player_to_json s.players []) in
  let rocks   = `List (Hashtbl.fold rock_to_json s.rocks []) in
  let s' = `Assoc [
    ("size"    , `List [`Float x; `Float y]);
    ("rad"     , `Float s.s_rad);
    ("ammo"    , ammo);
    ("bullets" , bullets);
    ("guns"    , guns);
    ("players" , players);
    ("rocks"   , rocks);
  ] in s' |> Yojson.Basic.to_string
