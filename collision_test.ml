open OUnit2
open Collision

let map () = create ()
let itoa i = string_of_int i

let entity id r pos =
  let open Stype in Player (id, r, pos)

let rec apply_many m f = function
| []     -> ()
| p :: t -> f m p; apply_many m f t

let update_many m l = apply_many m update l
let remove_many m l = apply_many m remove l  

let p0 = entity 0 49.0 (50.00, 50.00)
let p1 = entity 1 50.0 (50.00, 50.00)
let p2 = entity 2 1.0  (49.50, 49.50)
let p3 = entity 3 12.0 (55.50, 45.50)
let p4 = entity 4 1.0  (99.99, 100.0)
let p5 = entity 5 1.0  (999.0, 999.0)

let rec non_touching_chain acc = function
| 0 -> acc
| n -> non_touching_chain ((entity n 0.25 (float_of_int n, float_of_int n)) :: acc) (n - 1)

let rec touching_chain acc = function
| 0 -> acc
| n -> touching_chain ((entity n 0.75 (float_of_int n, float_of_int n)) :: acc) (n - 1)

let tests = [
  "create"            >:: (fun _ -> assert_equal 0 (List.length (all (map ()))));
  "test_no_collision" >:: (fun _ -> assert_equal 0 (List.length (test (map ()) p0)));
  "test_collision"    >:: (fun _ -> assert_equal 1 (List.length (let m = map () in update_many m [p0; p1]; all m)));
  "test_many"         >:: (fun _ -> assert_equal 6 (List.length (let m = map () in update_many m [p0; p1; p2; p3]; all m)));
  "test_remove"       >:: (fun _ -> assert_equal 3 (List.length (let m = map () in update_many m [p0; p1; p2; p3]; 
                                                                 remove m p3; all m)) ?printer:(Some string_of_int));
  "test_remove_all"   >:: (fun _ -> assert_equal 0 (List.length (let m = map () in update_many m [p0; p1; p2; p3];
                                                                 remove_many m [p0; p1; p2; p3]; all m)));
  "test_far"          >:: (fun _ -> assert_equal 0 (List.length (let m = map () in update_many m [p0; p5]; all m)));
  "test_many_far"     >:: (fun _ -> assert_equal 0 (List.length (let m = map () in update_many m [p0; p4; p5]; all m)));
  "stress_no_touch"   >:: (fun _ -> assert_equal 0 (List.length (let m = map () in 
                                                                 update_many m (non_touching_chain [] 10000); all m)));
  "stress_touch"      >:: (fun _ -> assert_equal 9999 (List.length (let m = map () in 
                                                                 update_many m (touching_chain [] 10000); all m)) ?printer:(Some string_of_int));
]

let suite = "Collision test suite" >::: tests

let _ = run_test_tt_main suite
