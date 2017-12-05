include Type

type t = int ref

let create () = ref 0

let create_guns i n = failwith "Unimplemented"

let create_ammo i n = failwith "Unimplemented"

let pistol i = 
  let g = {
    g_id   = "pistol" ^ (string_of_int !i);
    g_cd   = 0;
    g_own  = "";
    g_pos  = (0.00, 0.00); 
    g_rad  = 100.00;
    g_rate = 10;
    g_ammo = 20;
  } 
  
  
  
  
  
  let _ = i := !i + 1 in
  
