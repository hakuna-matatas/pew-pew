type pos = (float * float)
type dir = N | NE | E | SE | S | SW | W | NW

type g_id = string
type p_id = string
type r_id = int
type s_id = string

type ammo = {
  gun   : g_id; 
  count : int;
  a_pos : pos;
}

type bullet = {
  gun    : g_id;
  player : p_id;
  b_pos  : pos;
}

type gun = {
  g_id   : g_id;
  g_pos  : pos;
  rate   : float;
  ammo   : int;
  damage : int;
}

type player = {
  p_id  : p_id;
  p_pos : pos;
  hp    : int;
  dir   : dir;
  inv   : g_id list;
}

type rock = {
  r_id   : r_id;
  r_pos  : pos;
  radius : float;
}

type entity = 
| Rock   of (r_id * pos) 
| Bullet of (g_id * pos)
| Ammo   of (g_id * pos)
| Gun    of (g_id * pos)
| Player of (p_id * pos)

type map = (entity list) array array

type state = {
  s_id    : s_id;
  map     : map;
  time    : int;
  radius  : float;
  ammo    : ammo list
  bullets : bullet list
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
    ("gun", `String a.gun);
    ("pos", `List [`Float x; `Float y])
  ]

let bullet_to_json b =
  let x, y = b.b_pos in
  `Assoc [
    ("gun", `String b.gun);
    ("pos", `List [`Float x; `Float y])
  ]

let gun_to_json g_id g acc =
  let x, y = g.g_pos in
  let g' = `Assoc [
    ("id"     , `String g.g_id);
    ("rate"   , `Float  g.rate);
    ("ammo"   , `Float  g.ammo);
    ("damage" , `Int    g.damage);
    ("pos"    , `List   [`Float x; `Float y])
  ] in
  g' :: acc
  
let player_to_json p_id p acc =
  let x, y = p.p_pos in
  let p'   = `Assoc [
    ("id"  , `String p.p_id);
    ("hp"  , `Int    p.hp);
    ("dir" , `String (dir_to_json p.dir));
    ("inv" , `List   p.guns);
    ("pos" , `List   [`Float x; `Float y])
  ] in
  p' :: acc

let rock_to_json r_id r acc =
  let x, y = r.r_pos in
  let r'   = `Assoc [
    ("radius", `Float r.radius);
    ("pos", `List [`Float x; `Float y])
  ] in
  r' :: acc

let to_json s =
  let ammo    = `List (List.map ammo_to_json s.ammo) in
  let bullets = `List (List.map bullet_to_json s.bullets) in
  let guns    = `List (Hashtbl.fold gun_to_json s.guns []) in
  let players = `List (Hashtbl.fold player_to_json s.players []) in
  let rocks   = `List (Hashtbl.fold rock_to_json s.rocks []) in
  `Assoc [
    ("ammo"    , ammo);
    ("bullets" , bullets);
    ("guns"    , guns);
    ("players" , players);
    ("rocks"   , rocks);
  ]`
