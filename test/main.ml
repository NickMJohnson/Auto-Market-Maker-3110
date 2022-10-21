open OUnit2
open Database

let data_dir_prefix = "data" ^ Filename.dir_sep

let empty =
  Yojson.Basic.from_file (data_dir_prefix ^ "empty.json")
  |> Yojson.Basic.to_string

let bros =
  Yojson.Basic.from_file (data_dir_prefix ^ "3usersexample.json")
  |> Yojson.Basic.to_string

let db_name_test (name : string) (json : string) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (json |> from_json |> db_name) ~printer:Fun.id

let database_tests =
  [
    db_name_test {|db_name of empty is "empty"|} empty "empty";
    db_name_test {|db_name of bros is "3usersexample"|} bros "3usersexample";
    db_name_test {|db_name of [new_database "new"] is "new|}
      (Database.new_database "new" |> to_json)
      "new";
  ]

let tests = "Final project tests" >::: List.flatten [ database_tests ]
let _ = run_test_tt_main tests