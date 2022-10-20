open OUnit2
open Database

(*let db_name_test (name : string) (json : Yojson.Basic.t) (expected_output :
  string) : test = name >:: fun _ -> assert_equal expected_output (json |>
  from_json |> start_room) ~printer:Fun.id*)

let database_tests = []
let tests = "Final project tests" >::: List.flatten [ database_tests ]
let _ = run_test_tt_main tests