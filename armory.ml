include Type

let pistol i = 
  let g = {
    g_id   = "pistol" ^ (string_of_int !i);
    g_cd   = 0;
    g_own  = "";
    g_pos  = (0.00, 0.00); 
    g_rad  = 100.00;
    g_rate = 10;
    g_ammo = 20
  } in g
