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

let db_users_test (name : string) (json : string)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (users (from_json json))

let db_new_user_test (name : string) (json : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (users (new_user (from_json json) "Jack"))

let db_deposit_test (name : string) (json : string) (curr : string)
    (user : string) (amt : int) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (user_balance (deposit (from_json json) user curr amt) user curr)
    ~printer:string_of_int

let db_withdraw_test (name : string) (json : string) (curr : string)
    (user : string) (amt : int) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (user_balance (withdraw (from_json json) user curr amt) user curr)
    ~printer:string_of_int

let user_balance_test (name : string) (json : string) (user : string)
    (curr : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (user_balance (from_json json) user curr)
    ~printer:string_of_int

let database_tests =
  [
    db_name_test {|db_name of empty is "empty"|} empty "empty";
    db_name_test {|db_name of bros is "3usersexample"|} bros "3usersexample";
    db_name_test {|db_name of [new_database "new"] is "new|}
      (Database.new_database "new" |> to_json)
      "new";
    db_users_test {|users in empty is []|} empty [];
    db_users_test {|users in bros is [Tony, Nick, Anthony]|} bros
      [ "Tony"; "Nick"; "Anthony" ];
    db_new_user_test {|users in new_user Jack in empty is [Jack]|} empty
      [ "Jack" ];
    db_new_user_test
      {|users in new_user Jack in 3usersexample is [Jack, Tony, Nick, Anthony]|}
      bros
      [ "Jack"; "Tony"; "Nick"; "Anthony" ];
    db_deposit_test {|deposit 100 USD to Nick's USD balance is 100|} bros "USD"
      "Nick" 100 100;
    db_deposit_test {|deposit 50 USD to Nick's USD balance is 50|} bros "usd"
      "Nick" 50 50;
    db_deposit_test {|deposit 70 BRB to Nick's USD balance is 71|} bros "brb"
      "Nick" 70 71;
    db_deposit_test {|deposit 100 USD to Nick's USD balance is 100|} bros "BRB"
      "Tony" 10 20010;
    db_withdraw_test {|withdraw 400 USD to Tony's USD balance is 1000|} bros
      "USD" "Tony" 400 1000;
    db_withdraw_test {|withdraw 10000 BRB to Tony's BRB balance is 50|} bros
      "brb" "Tony" 10000 10000;
    db_withdraw_test {|withdraw 50 USD to Nick's BRB balance is -50|} bros "usd"
      "Nick" 50 (-50);
    db_withdraw_test {|withdraw 70 BRB to Nick's BRB balance is -69|} bros "brb"
      "Nick" 70 (-69);
    user_balance_test {|user balance of Nick's USD is 0|} bros "Nick" "usd" 0;
    user_balance_test {|user balance of Tony's USD is 1400|} bros "Tony" "USD"
      1400;
    user_balance_test {|user balance of Anthony's USD is 1500000|} bros
      "Anthony" "uSd" 1500000;
    user_balance_test {|user balance of Nick's BRB is 1|} bros "Nick" "brb" 1;
    user_balance_test {|user balance of Tony's BRB is 20000|} bros "Tony" "BRB"
      20000;
    user_balance_test {|user balance of Anthony's BRB is 2000000|} bros
      "Anthony" "bRb" 2000000;
  ]

let tests = "Final project tests" >::: List.flatten [ database_tests ]
let _ = run_test_tt_main tests