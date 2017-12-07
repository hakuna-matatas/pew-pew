let state_test = {
  id = 0;
  name = "test_state";
  size = (5000,5000);
  radius = 2000;
  ammo = [{
    a_type = "pistol";
    a_pos = (1200,1200);
    a_rad = 50;
    a_amt = 5;
  }];
  bullets = [{
    b_type = "pistol";
    b_pos = (1550,1550);
    b_rad = 5
  }];
  players = [{
    p_id = 1;
    p_name = "Alan";
    p_hp = 5;
    p_pos = (1500,1500);
    p_rad = 20;
    p_dir = SE;
    p_inv = [10]
  }];
  guns = [{
    g_id = 10;
    g_own = 1;
    g_pos = (1250, 1250);
    g_rad = 10;
    g_type = "pistol";
    g_ammo = 10
   }];
  rocks = [{
    r_pos = (1340, 1340);
    r_rad = 3
  }];
}
