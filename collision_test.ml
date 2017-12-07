open OUnit2
open Collision

let map = create ()

let itoa i = string_of_int i

let entity id r pos =
  let open Stype in Player (id, r, pos)

let player = entity 0 50.0 (50.00, 50.00)
let player' = entity 1 50.0 (50.00, 50.00)

let simple_tests = [
  "create"            >:: (fun _ -> assert_equal 0 (List.length (all map)));
  "test_no_collision" >:: (fun _ -> assert_equal 0 (List.length (test map player)));
  "test_collision"    >:: (fun _ -> assert_equal 1 (List.length (update map player'; update map player; all map)));
]

let suite = "Collision test suite" >::: List.flatten ([simple_tests])

let _ = run_test_tt_main suite
