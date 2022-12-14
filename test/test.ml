open OUnit2
open Database 

(*Testing Plan: Our Testing is split into four segments. The first three segments are OUnit Tests and 
the last is an interface testing plan. First, We will test many combinations of the "banking" component 
of our project. This includes validating users can add themselves to a database, deposit, withdraw, 
and save the database. This also tests our functionality for retreiving information from the database. 
The next phase of our testing plan tests simple and easily verifiable function calls on all functionality 
so we can see the integration of the trading portion of our project. Finally, Phase three simulates multiple
users path through the trading platform and validates that trades are filled when Buy/Sell prices are equal,
partially filled when one amount is greater than the other at the same price, and filled at different levels 
starting with the best market price and going up to either the price limit or amount limit the user defined
in thier order. The last and most important phase of our testing plan is testing the user interface. 
This involved testing the path through every possible screen/input, Testing for invalid input protection, 
Validating graphs accuracy and updates, validating order functionality in the same way as our test, 
and Validating that the json file is accuratly updated upon user logout. 

   *)

let data_dir_prefix = "data" ^ Filename.dir_sep
let empty =
  Yojson.Basic.from_file (data_dir_prefix ^ "empty.json")
  |> Yojson.Basic.to_string

let bros =
  Yojson.Basic.from_file (data_dir_prefix ^ "3usersexample.json")
  |> Yojson.Basic.to_string

let order_example =
  from_json(Yojson.Basic.from_file (data_dir_prefix ^ "order_example.json")
  |> Yojson.Basic.to_string)

let update_json (db : Database.t) =
  let path = "data/" ^ db_name db ^ ".json" in
  Yojson.Basic.to_file path (db |> to_json |> Yojson.Basic.from_string);
  print_endline("database updated");
  db


let db_name_test (name : string) (json : string) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (json |> from_json |> db_name) ~printer:Fun.id

let db_users_test (name : string) (json : string)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (users (from_json json))

let db_new_user_test (name : string) (json : string) (user : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (users (new_user (from_json json) user))

let db_deposit_test (name : string) (json : string) (curr : string)
    (user : string) (amt : int) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (user_balance (deposit user curr amt (from_json json) ) user curr)
    ~printer:string_of_int


let db_withdraw_test (name : string) (json : string) (curr : string)
    (user : string) (amt : int) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (user_balance (withdraw user curr amt (from_json json)) user curr)
    ~printer:string_of_int


let user_balance_test (name : string) (json : string) (user : string)
    (curr : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (user_balance (from_json json) user curr)
    ~printer:string_of_int

let buy_order_balance_test (name : string) (db : t) (user : string) (curr : string)
(amt : int) (rate: float) (expected_output : int) : test = 
name >:: fun _ ->
assert_equal expected_output
(user_balance (buy_order user amt (rate : float) (db)) user curr)
~printer:string_of_int

let sell_order_balance_test (name : string) (db : t) (user : string) (curr : string)
(amt : int) (rate: float) (expected_output : int) : test = 
name >:: fun _ ->
assert_equal expected_output
(user_balance (sell_order user amt (rate : float) (db)) user curr)
~printer:string_of_int

let order_book_tests (name : string) (json : string)
(expected_buy_output : int) (expected_sell_output : int) : test =
name >:: fun _ ->
assert_equal (expected_buy_output,expected_sell_output)
((List.length (from_json json).orders.buy_orders),(List.length (from_json json).orders.sell_orders))


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
    db_new_user_test {|users in new_user Jack in empty is [Jack]|} empty "Jack"
      [ "Jack" ];
    db_new_user_test
      {|users in new_user Jack in 3usersexample is [Jack, Tony, Nick, Anthony]|}
      bros "Jack"
      [ "Jack"; "Tony"; "Nick"; "Anthony" ];
    db_deposit_test {|deposit 100 USD to Nick's USD balance is 100|} bros "usd"
      "Nick" 100 100;
    db_deposit_test {|deposit 50 USD to Nick's USD balance is 50|} bros "usd"
      "Nick" 50 50;
    db_deposit_test {|deposit 70 BRB to Nick's USD balance is 71|} bros "brb"
      "Nick" 70 71;
    db_deposit_test {|deposit 100 USD to Nick's USD balance is 100|} bros "brb"
      "Tony" 10 20010;
    db_withdraw_test {|withdraw 400 USD to Tony's USD balance is 1000|} bros
      "usd" "Tony" 400 1000;
    db_withdraw_test {|withdraw 10000 BRB to Tony's BRB balance is 50|} bros
      "brb" "Tony" 10000 10000;
    db_withdraw_test {|withdraw 50 USD to Nick's BRB balance is -50|} bros "usd"
      "Nick" 50 (-50);
    db_withdraw_test {|withdraw 70 BRB to Nick's BRB balance is -69|} bros "brb"
      "Nick" 70 (-69);
    user_balance_test {|user balance of Nick's USD is 0|} bros "Nick" "usd" 0;
    user_balance_test {|user balance of Tony's USD is 1400|} bros "Tony" "usd"
      1400;
    user_balance_test {|user balance of Anthony's USD is 1500000|} bros
      "Anthony" "usd" 1500000;
    user_balance_test {|user balance of Nick's BRB is 1|} bros "Nick" "brb" 1;
    user_balance_test {|user balance of Tony's BRB is 20000|} bros "Tony" "brb"
      20000;
    user_balance_test {|user balance of Anthony's BRB is 2000000|} bros
      "Anthony" "brb" 2000000;
  ]
let tests1 =[
    "new_database" >:: (fun _ ->
        let db = new_database "test_db" in
        let name = db_name db in
        assert_equal ~printer:(fun x -> x) name "test_db");
    "new_user" >:: (fun _ ->
        let db = new_database "test_db" in
        let db' = new_user db "user1" in
        let db'' = new_user db' "user2" in 
        let users = users db'' in
        assert_equal users ["user2";"user1"]);
    "deposit" >:: (fun _ ->
        let db = new_database "test_db" in
        let db' = new_user db "user1" in
        let db'' = deposit "user1" "usd" 100 db' in
        let balance = user_balance db'' "user1" "usd" in
        assert_equal balance 100);
    "withdraw" >:: (fun _ ->
        let db = new_database "test_db" in
        let db' = new_user db "user1" in
        let db'' = deposit "user1" "usd" 100 db' in
        let db''' = withdraw "user1" "usd" 50 db'' in
        let balance = user_balance db''' "user1" "usd" in
        assert_equal balance 50);
    "buy_order" >:: (fun _ ->
        let db = new_database "test_db" in
        let db' = new_user db "user1" in
        let db'' = buy_order "user1" 100 0.8 db' in
        let orders = db''.orders.buy_orders in
        assert_equal orders [ { user = "user1"; amount = 100; rate = 0.8 } ]);
    "sell_order" >:: (fun _ ->
        let db = new_database "test_db" in
        let db' = new_user db "user1" in
        let db'' = sell_order "user1" 100 0.9 db' in
        let orders = db''.orders.sell_orders in
        assert_equal orders [ { user = "user1"; amount = 100; rate = 0.9 } ]);
    "from_json" >:: (fun _ ->
        let db = new_database "test_db" in
        let db' = new_user db "user1" in
        let db_json = to_json db' in
        let db'' = from_json db_json in
        let users = users db'' in
        assert_equal  users ["user1"]);

  ]

 let db1 = new_user order_example "Tony" 
 let db2 = new_user db1 "Nick"
 let db3 = new_user db2 "Anthony"
 let db4 = db3 |> deposit "Tony" "usd" 100 |> deposit "Nick" "usd" 100 |> deposit "Anthony" "usd" 100 
 let db5 = db4 |> deposit "Tony" "brb" 200 |> deposit "Nick" "brb" 200 |> deposit "Anthony" "brb" 200
 let db6 = buy_order "Nick" 100 0.5 db5
 let db7 = sell_order "Tony" 100 0.6 db6
 let db8 = sell_order "Tony" 100 0.5 db7
 let db9 = buy_order "Nick" 50 0.6 db8
 let db10 = buy_order "Anthony" 50 0.7 db9


let order_tests = [
  db_name_test {|db_name of [new_database "order_example"] is "order_example"|} 
  (to_json order_example ) "order_example";
  db_users_test {|users in order_example is []|} (to_json order_example) [];
  db_new_user_test {|users in new_user Tony in order_example is [Tony]|} 
  (to_json order_example) "Tony" ["Tony"];
  db_new_user_test {|users in new_user Nick in order_example is [Nick; Tony]|} 
  (to_json db1) "Nick" [ "Nick"; "Tony" ];
  db_new_user_test {|users in new_user Anthony in order_example is [Anthony; Nick; Tony]]|} 
  (to_json db2) "Anthony" [ "Anthony" ; "Nick" ; "Tony" ];
  user_balance_test {|user balance of Nick's BRB is 0|} (to_json db3) "Nick" "brb" 0;
  user_balance_test {|user balance of Nick's USD is 0|} (to_json db3) "Nick" "usd" 0;
  user_balance_test {|user balance of Tonys's BRB is 0|} (to_json db3) "Tony" "brb" 0;
  user_balance_test {|user balance of Tony's USD is 0|} (to_json db3) "Tony" "usd" 0;
  user_balance_test {|user balance of Anthony's BRB is 0|} (to_json db3) "Anthony" "brb" 0;
  user_balance_test {|user balance of Anthony's USD is 0|} (to_json db3) "Anthony" "usd" 0;
  db_deposit_test {|deposit 100 USD to Nick's USD balance is 100|} (to_json db3) "usd"
      "Nick" 100 100;
  db_deposit_test {|deposit 100 USD to Tony's USD balance is 100|} (to_json db3)"usd"
      "Tony" 100 100;
  db_deposit_test {|deposit 100 USD to Anthony's USD balance is 100|}(to_json db3)"usd"
      "Anthony" 100 100;
db_deposit_test {|deposit 200 USD to Nick's BRB balance is 200|}(to_json db4) "brb"
      "Nick" 200 200;
db_deposit_test {|deposit 200 USD to Tony's BRB balance is 200|} (to_json db4) "brb"
      "Tony" 200 200;
db_deposit_test {|deposit 200 USD to Anthony's BRB balance is 200|} (to_json db4)"brb"
      "Anthony" 200 200;
order_book_tests {|There are 0 buy orders and 0 sell orders in orderbook|} (to_json db5) 0 0;
buy_order_balance_test {|Nick place buy order for 100 BRB at price 0.5 Usd/bnb 
initial BRB balance = 100 final BRB balance = 100|} db5 "Nick" "brb" 100 0.5 200;
order_book_tests {|There is 1 buy order and 0 sell orders in orderbook|} (to_json db6) 1 0;
sell_order_balance_test {|Tony place sell order for 100 BRB at price 0.6 Usd/bnb 
initial USD balance = 100 final USD balance = 100|} db6 "Tony" "brb" 100 0.6 200;
order_book_tests {|There is 1 buy order and 1 sell orders in orderbook|} (to_json db7) 1 1;
sell_order_balance_test {|Tony place sell order for 100 BRB at price 0.5 Usd/bnb 
initial BRB balance = 200 final BRB balance = 100|} db7 "Tony" "brb" 100 0.5 100;
order_book_tests {|There are 0 buy orders and 1 sell orders in orderbook|} (to_json db8) 0 1;
user_balance_test {|user balance of Nick's BRB is 300|} (to_json db8) "Nick" "brb" 300;
user_balance_test {|user balance of Nick's USD is 50|} (to_json db8) "Nick" "usd" 50;
user_balance_test {|user balance of Tonys's BRB is 100|} (to_json db8) "Tony" "brb" 100;
user_balance_test {|user balance of Tony's USD is 150|} (to_json db8) "Tony" "usd" 150;
buy_order_balance_test {|Nick place buy order for 50 BRB at price 0.6 Usd/Bnb 
initial BRB balance = 300 final BRB balance = 350|} db8 "Nick" "brb" 50 0.6 350;
order_book_tests {|There are 0 buy orders and 1 sell orders in orderbook|} (to_json db9) 0 1;
user_balance_test {|user balance of Nick's USD is 20|} (to_json db9) "Nick" "usd" 20;
user_balance_test {|user balance of Tonys's BRB is 50|} (to_json db9) "Tony" "brb" 50;
user_balance_test {|user balance of Tony's USD is 180|} (to_json db9) "Tony" "usd" 180;
buy_order_balance_test {|Anthony place buy order for 50 BRB at price 0.7 Usd/Bnb 
initial BRB balance = 200 final BRB balance = 250|} db9 "Anthony" "brb" 50 0.7 250;
order_book_tests {|There are 0 buy orders and 0 sell orders in orderbook|} (to_json db10) 0 0;
user_balance_test {|user balance of Anthonys's USD is 70|} (to_json db10) "Anthony" "usd" 70;
user_balance_test {|user balance of Tonys's BRB is 0|} (to_json db10) "Tony" "brb" 0;
user_balance_test {|user balance of Tony's USD is 210|} (to_json db10) "Tony" "usd" 210;

]

let tests = "Final project tests" >:::  List.flatten [database_tests; order_tests; tests1]
let _ = run_test_tt_main tests;