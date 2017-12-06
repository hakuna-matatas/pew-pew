(*                *
  * Map Generation *
  *                *)

(* How many entities the game starts with *)
let initial_rocks = 50
let initial_guns  = 10
let initial_ammo  = 50

(* Ammo per pack *)
let ammo_count () = 10 + (Random.int 10) 

(* HP per player *)
let player_hp = 100

(*                *
  * Entity Scaling *
  *                *)

(* Ring of death initial radius. Starts from map center. *)
let ring_radius = 6000.00

(* Map dimensions. *)
let map_width = 5000.00
let map_height = 5000.00

(* Entity radii. *)
let player_radius = 30.00
let ammo_radius = 20.00
let gun_radius  = 20.00
let rock_radius = 60.00

(*                *
  * World Stepping *
  *                *)

(* Rate at which cooldown timer decreases *)
let gun_cd_rate      = 1

(* Rate at which ring of death radius decreases *)
let constrict_rate   = 0.05

(* Interval between ammo drops *)
let ammo_spawn_cd    = 200

(* Number of ammo packs to drop per interval *)
let ammo_spawn_count = 10

(* Interval between gun drops *)
let gun_spawn_cd     = 1000

(* Number of guns to drop per interval *)
let gun_spawn_count  = 10

(* Maximum squared distance a player can travel before flagged as cheating *)
let max_sq_distance = 10000.00
