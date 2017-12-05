type pos = (float * float)
type rad = float
type dir = N | NE | E | SE | S | SW | W | NW
type id  = string

type ammo = {
  a_gun : id; 
  a_pos : pos;
  a_rad : rad;
  a_amt : int;
}

type bullet = {
  b_gun  : id;
  b_own  : id;
  b_pos  : pos;
  b_rad  : rad;
  b_dmg  : int;
  b_step : bullet ->  bullet;
}

type player = {
  p_id  : id;
  p_hp  : int;
  p_pos : pos;
  p_rad : rad;
  p_dir : dir;
  p_inv : id list;
}

type gun = {
  g_id   : id;
  g_cd   : int;
  g_own  : id;
  g_pos  : pos;
  g_rad  : rad;
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

let dir_of_json j = failwith "Unimplemented"

let ammo_to_json a = 
  let x, y = a.a_pos in
  `Assoc [
    ("gun"    , `String a.a_gun);
    ("amount" , `Int    a.a_amt);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad"    , `Float  a.a_rad)
  ]

let ammo_of_json j = failwith "Unimplemented"

let bullet_to_json b =
  let x, y = b.b_pos in
  `Assoc [
    ("gun" , `String b.b_gun);
    ("rad" , `Float  b.b_rad);
    ("pos" , `List   [`Float x; `Float y])
  ]

let bullet_of_json j = failwith "Unimplemented"

let gun_to_json g =
  let x, y = g.g_pos in
  `Assoc [
    ("id"     , `String g.g_id);
    ("owner"  , `String g.g_own);
    ("ready"  , `Bool   (g.g_cd = 0));
    ("ammo"   , `Int    g.g_ammo);
    ("pos"    , `List   [`Float x; `Float y]);
    ("rad" , `Float  g.g_rad)
  ]
  
let gun_of_json j = failwith "Unimplemented"

let player_to_json p =
  let x, y = p.p_pos in
  let inv' = List.map (fun g_id -> `String g_id) p.p_inv in
  `Assoc [
    ("id"  , `String p.p_id);
    ("hp"  , `Int    p.p_hp);
    ("dir" , dir_to_json p.p_dir);
    ("inv" , `List   inv');
    ("pos" , `List   [`Float x; `Float y]);
    ("rad" , `Float  p.p_rad)
  ]

let player_of_json j = failwith "Unimplemented"

let rock_to_json r =
  let x, y = r.r_pos in
  `Assoc [
    ("pos" , `List  [`Float x; `Float y]);
    ("rad" , `Float r.r_rad)
  ]

let rock_of_json j = failwith "Unimplemented"
