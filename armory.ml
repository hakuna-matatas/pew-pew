open Type

let buffer = 5.0

(* Returns coordinates [d] units away from [x, y] in direction [dir]. *)
let spawn (x, y) d dir = 
  let d' = d *. (sqrt 2.0) /. 2.0 in match dir with
  | N  -> (x       , y -. d )
  | NE -> (x +. d' , y -. d')
  | E  -> (x +. d  , y      )
  | SE -> (x +. d' , y +. d')
  | S  -> (x       , y +. d )
  | SW -> (x -. d' , y +. d')
  | W  -> (x -. d  , y      )
  | NW -> (x -. d' , y -. d')

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

let all = [pistol]

let create () = 
  let n = Random.int (List.length all) in
  List.nth all n
