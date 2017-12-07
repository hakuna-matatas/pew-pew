open Stype

let buffer = 5.0

(* Returns coordinates [d] units away from [x, y] in direction [dir]. *)
let spawn (x, y) d dir =
  let d' = d *. (sqrt 2.0) /. 2.0 in match dir with
  | N  -> (x       , y +. d )
  | NE -> (x +. d' , y +. d')
  | E  -> (x +. d  , y      )
  | SE -> (x +. d' , y -. d')
  | S  -> (x       , y -. d )
  | SW -> (x -. d' , y -. d')
  | W  -> (x -. d  , y      )
  | NW -> (x -. d' , y +. d')

let fire_shotgun p =
  let rad = 10.0 in
  let speed = 0.7 in
  [{
    b_id   = 0;
    b_type = "Shotgun";
    b_own  = p.p_id;
    b_pos  = spawn p.p_pos (p.p_rad +. rad +. buffer) p.p_dir;
    b_rad  = rad;
    b_dmg  = 5;
    b_time = 0;
    b_step = (fun b ->
        {b with
         b_dmg = if b.b_time mod 70 = 0 then b.b_dmg - 1 else b.b_dmg;
         b_pos = spawn b.b_pos speed p.p_dir;
         b_time = b.b_time + 1
        });
  }]

let shotgun =
  {
    g_id   = 1;
    g_cd   = 1;
    g_own  = 0;
    g_pos  = (0.00, 0.00);
    g_rad  = 100.00;
    g_type = "Shotgun";
    g_rate = 50;
    g_ammo = 10;
    g_fire = fire_shotgun
  }

let fire_mine p =
  let rad = 13.0 in
  [{
    b_id = 0;
    b_type = "Mine";
    b_own = p.p_id;
    b_pos = spawn p.p_pos (p.p_rad +. rad +. buffer) p.p_dir;
    b_rad = rad;
    b_dmg = 30;
    b_time = 0;
    b_step = (fun b -> {b with b_time = b.b_time + 1})
  }]

let mine =
  {
    g_id   = 1;
    g_cd   = 1;
    g_own  = 0;
    g_pos  = (0.00, 0.00);
    g_rad  = 40.00;
    g_type = "Mine";
    g_rate = 50;
    g_ammo = 10;
    g_fire = fire_mine
  }

let fire_pistol p =
  let rad = 5.0 in
  let speed = 0.5 in
  [{
    b_id   = 0;
    b_type = "Pistol";
    b_own  = p.p_id;
    b_pos  = spawn p.p_pos (p.p_rad +. rad +. buffer) p.p_dir;
    b_rad  = rad;
    b_dmg  = 5;
    b_time = 0;
    b_step = (fun b ->
        {b with
         b_pos = spawn b.b_pos speed p.p_dir;
         b_time = b.b_time + 1
        });
  }]

let pistol =
  {
    g_id   = 0;
    g_cd   = 0;
    g_own  = 0;
    g_pos  = (0.00, 0.00);
    g_rad  = 100.00;
    g_type = "Pistol";
    g_rate = 10;
    g_ammo = 20;
    g_fire = fire_pistol
  }

let fire_octo p =
  let rad = 5.0 in
  let speed = 0.5 in
  let b dir = {
    b_id   = 0;
    b_type = "Octo";
    b_own  = p.p_id;
    b_pos  = spawn p.p_pos (p.p_rad +. rad +. buffer) dir;
    b_rad  = rad;
    b_dmg  = 5;
    b_time = 0;
    b_step = (fun b ->
        {b with
         b_pos = spawn b.b_pos speed dir;
         b_time = b.b_time + 10});
  } in [b N; b NE; b E; b SE; b S; b SW; b W; b NW]

let octo =
  {
    g_id   = 0;
    g_cd   = 0;
    g_own  = 0;
    g_pos  = (0.00, 0.00);
    g_rad  = 100.00;
    g_type = "Octo";
    g_rate = 40;
    g_ammo = 20;
    g_fire = fire_octo
  }

let all = [pistol; shotgun; mine; octo]

let create () =
  let n = Random.int (List.length all) in
  List.nth all n
