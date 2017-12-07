type pos  = (float * float)
type rad  = float
type dir  = N | NE | E | SE | S | SW | W | NW
type id   = int
type name = string

type ammo = {
  a_id   : id;
  a_type : name;
  a_pos  : pos;
  a_rad  : rad;
  a_amt  : int;
}

type bullet = {
  b_id   : id;
  b_type : name;
  b_own  : id;
  b_pos  : pos;
  b_rad  : rad;
  b_dmg  : int;
  b_time : int;
  b_step : bullet ->  bullet;
}

type player = {
  p_id   : id;
  p_name : name;
  p_hp   : int;
  p_pos  : pos;
  p_rad  : rad;
  p_dir  : dir;
  p_inv  : id list;
  p_last : name
}

type gun = {
  g_id   : id;
  g_cd   : int;
  g_own  : id;
  g_pos  : pos;
  g_rad  : rad;
  g_type : name;
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

(* Schema:
 *
 *  {
 *    "NW"
 *  }
 *
 * Can be "N", "NE", "E", etc.
 *)
val dir_to_json : dir -> Yojson.Basic.json

(* Schema:
 *
 *  {
 *    "gun"    : "gun_type", // e.g. "Bazooka"
 *    "amount" : 5,
 *    "pos"    : [0.56, 100.32],
 *    "rad"    : 6.56
 *  }
 *)
val ammo_to_json : ammo -> Yojson.Basic.json
val ammo_to_entity : ammo -> entity

(* Schema:
 *
 *  {
 *    "gun" : "gun_type", // e.g. "Pistol"
 *    "rad" : 5.32,
 *    "pos" : [99.52, 33.23]
 *  }
 *)
val bullet_to_json : bullet -> Yojson.Basic.json
val bullet_to_entity : bullet -> entity

(* Schema:
 *
 *  {
 *    "id"    : "gun_id",
 *    "owner" : "player_id",
 *    "type"  : "Pistol"
 *    "ammo"  : 5,
 *    "pos"   : [32.55, 99.22],
 *    "rad"   : 5.00
 *  }
 *)
val gun_to_json : gun -> Yojson.Basic.json
val gun_to_entity : gun -> entity

(* Schema:
 *
 *  {
 *    "id"   : "player_id",
 *    "name" : "AGao"
 *    "hp"   : 255,
 *    "dir"  : "NW",
 *    "inv"  : ["gun_id0", "gun_id1"],
 *    "pos"  : [9999.99, 0.00],
 *    "rad"  : 34.22,
 *    "last" : "Pistol"
 *  }
 *)
val player_to_json : player -> Yojson.Basic.json
val player_to_entity : player -> entity

(* Schema:
 *
 *  {
 *    "pos" : [3232.55, 55.55],
 *    "rad" : 56.99
 *  }
 *)
val rock_to_json : rock -> Yojson.Basic.json
val rock_to_entity : rock -> entity
