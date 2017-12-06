type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW
type id  = string

type ammo = {
  a_id  : id;
  a_gun : id;
  a_pos : pos;
  a_rad : rad;
  a_amt : int;
}

type bullet = {
  b_id   : id;
  b_gun  : id;
  b_own  : id;
  b_pos  : pos;
  b_rad  : rad;
  b_dmg  : int;
  b_step : bullet ->  bullet;
}

type player = {
  p_id   : id;
  p_name : id;
  p_hp   : int;
  p_pos  : pos;
  p_rad  : rad;
  p_dir  : dir;
  p_inv  : id list;
}

type gun = {
  g_id   : id;
  g_cd   : int;
  g_own  : id;
  g_pos  : pos;
  g_rad  : rad;
  g_type : id;
  g_rate : int;
  g_ammo : int;
  g_fire : player -> bullet list;
}

type rock = {
  r_id  : id;
  r_pos : pos;
  r_rad : rad;
}

type entity =
| Rock   of (id * rad * pos)
| Bullet of (id * rad * pos)
| Ammo   of (id * rad * pos)
| Gun    of (id * rad * pos)
| Player of (id * rad * pos)

let dir_to_json d = let s = match d with
| N -> "N" | NE -> "NE" | E -> "E" | SE -> "SE"
| S -> "S" | SW -> "SW" | W -> "W" | NW -> "NW"
in `String s

let ammo_to_json a =
  let x, y = a.a_pos in
  `Assoc [
    ("gun"    , `String a.a_gun);
    ("amount" , `Int    a.a_amt);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad"    , `Float  a.a_rad)
  ]

let ammo_to_entity a = Ammo (a.a_id, a.a_rad, a.a_pos)

let bullet_to_json b =
  let x, y = b.b_pos in
  `Assoc [
    ("gun" , `String b.b_gun);
    ("rad" , `Float  b.b_rad);
    ("pos" , `List   [`Float x; `Float y])
  ]

let bullet_to_entity b = Bullet (b.b_id, b.b_rad, b.b_pos)

let gun_to_json g =
  let x, y = g.g_pos in
  `Assoc [
    ("id"     , `String g.g_type);
    ("owner"  , `String g.g_own);
    ("ready"  , `Bool   (g.g_cd = 0));
    ("ammo"   , `Int    g.g_ammo);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad" , `Float  g.g_rad)
  ]

let gun_to_entity g = Gun (g.g_id, g.g_rad, g.g_pos)

let player_to_json p =
  let x, y = p.p_pos in
  let inv' = List.map (fun g_id -> `String g_id) p.p_inv in
  `Assoc [
    ("id"  , `String p.p_name);
    ("hp"  , `Int    p.p_hp);
    ("dir" , dir_to_json p.p_dir);
    ("inv" , `List   inv');
    ("pos" , `List   [`Float x; `Float y]);
    ("rad" , `Float  p.p_rad)
  ]

let player_to_entity p = Player (p.p_id, p.p_rad, p.p_pos)

let rock_to_json r =
  let x, y = r.r_pos in
  `Assoc [
    ("pos" , `List  [`Float x; `Float y]);
    ("rad" , `Float r.r_rad)
  ]

let rock_to_entity r = Rock (r.r_id, r.r_rad, r.r_pos)
