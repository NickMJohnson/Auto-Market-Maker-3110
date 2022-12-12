
open Database
open Order

type object_phrase = string list

let data_dir_prefix = "data" ^ Filename.dir_sep

type command =
  | NewDatabase of object_phrase
  | Database of object_phrase
  | Login of object_phrase
  | New_User of object_phrase
  | Home
  | Account
  | Admin
  | Deposit of object_phrase
  | Withdraw of object_phrase
  | View_Bal
  | View_Users
  | Order 
  | Graph 
  | Quit

exception Empty
exception Malformed

let com z =
  match String.lowercase_ascii z with
  | "" -> raise Empty
  | "quit" -> Quit
  | "home" -> Home
  | "account" -> Account
  | "admin" -> Admin
  | "view_balance" -> View_Bal
  | "view_users" -> View_Users
  | "order" -> Order
  | "graph" -> Graph
  | _ -> raise Malformed

let coms z sl =
  let len = List.length sl in
  match String.lowercase_ascii z with
  | "deposit" -> Deposit sl
  | "withdraw" -> Withdraw sl
  | _ when len > 1 -> raise Malformed
  | "" -> raise Empty
  | "new_database" -> NewDatabase sl
  | "database" -> Database sl
  | "login" -> Login sl
  | "new_user" -> New_User sl
  | _ -> raise Malformed

let redo = "please try again thank you!"
let string_split s = List.filter (fun x -> x <> "") (String.split_on_char ' ' s)

let parser s =
  match s with
  | [] -> raise Empty
  | [ h ] -> com h
  | h :: t -> coms h t

(* Create New Json File under data folder. Saved as data/jsonfile.json. The
   open_out and close_out function was found from Ocaml library documentation.
   Source: https://ocaml.org/docs/file-manipulation *)
let new_db_file filename json_string =
  let create_file = open_out ("data/" ^ filename ^ ".json") in
  Printf.fprintf create_file "%s\n" json_string;
  close_out create_file

(* Write Json to file. Updating db values*)
let update_json (db : Database.t) =
  let path = "data/" ^ db_name db ^ ".json" in
  Yojson.Basic.to_file path (db |> to_json |> Yojson.Basic.from_string)

let parse s = parser (string_split s)

let rec print_users stl =
  match stl with
  | [] -> "no users"
  | [ h ] -> h
  | h :: t -> h ^ " , " ^ print_users t

let rec home (db : Database.t) (u : string) =
  print_string
    "\n\n \n    Pleae select a menu \n\n    - Admin\n\n    - Account\n";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      home db u
  | inp ->
      (if "admin" = String.lowercase_ascii inp then admin db u
      else if "account" = String.lowercase_ascii inp then account db u
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"));
      home db u

and view_users db u =
  print_string (print_users (users db));
  admin db u

and admin db u =
  print_string
    "\n\n\
    \ \n\
    \    Pleae type your preference to select a menu \n\n\
    \    - View Users\n\n\
    \    - Home\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      admin db u
  | inp ->
      (if "view users" = String.lowercase_ascii inp then view_users db u
      else if "home" = String.lowercase_ascii inp then home db u
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"));
      admin db u

and quit db u =
  print_endline "Goodbye ";
  print_string u;
  update_json db;
  exit 0

and withdraw__ db u curr =
  print_endline "How much would you like to withdraw:\n";
  print_endline "Type an amount \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      withdraw__ db u curr
  | inp -> account (withdraw u curr (int_of_string inp) db) u

and withdraw_ db u =
  print_endline "What would you like to withdrawl?:\n";
  print_endline "Type brb to withdraw brb or usd to withdraw usd \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      withdraw_ db u
  | inp ->
      (if "brb" = String.lowercase_ascii inp then
       withdraw__ db u (String.lowercase_ascii inp)
      else if "usd" = String.lowercase_ascii inp then
        withdraw__ db u (String.lowercase_ascii inp)
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"));
      withdraw_ db u

and deposit__ db u curr =
  print_endline "How much would you like to deposit:\n";
  print_endline "Type an amount \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      deposit__ db u curr
  | inp -> account (deposit u curr (int_of_string inp) db) u

and deposit_ db u =
  print_endline "What would you like to deposit?:\n";
  print_endline "Type brb to deposit brb or usd to deposit usd \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      deposit_ db u
  | inp ->
      (if "brb" = String.lowercase_ascii inp then
       deposit__ db u (String.lowercase_ascii inp)
      else if "usd" = String.lowercase_ascii inp then
        deposit__ db u (String.lowercase_ascii inp)
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"));
      deposit_ db u

and sell__order db u rate=
  print_endline "Your BRB balance is:  ";
  print_int (user_balance db u "brb");
  print_endline " \n";
  print_endline "How much of this balance would you like to sell for USD\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      sell__order db u rate
  | inp -> account (sell_order u (int_of_string inp) rate db) u
  

and buy__order db u rate=
  print_string "Your USD balance is:  ";
  print_int (user_balance db u "usd");
  print_endline " \n";
  print_endline "How many BRB would you like to buy?\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      buy__order db u rate
  | inp -> account (buy_order u (int_of_string inp) rate db) u

and order__ db u curr= 
  print_endline " \n";
  print_endline "What is you asking price in USD per BRB :\n";
  print_endline "Type an amount Eg: 0.51\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      order__ db u curr
  | inp -> let rate = (float_of_string (String.lowercase_ascii inp)) in 
      (if rate > 0. && rate < 1.0 then
        if curr = "buy" then buy__order db u rate
        else if curr = "sell" then sell__order db u rate
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"));
      order__ db u curr

and order_ db u =
  print_endline "Would you like to place a Buy or Sell?:\n";
  print_endline "Type Buy to buy brb or sell to sell brb \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      order_ db u
  | inp ->
      (if "buy" = String.lowercase_ascii inp then
       order__ db u (String.lowercase_ascii inp)
      else if "sell" = String.lowercase_ascii inp then
        order__ db u (String.lowercase_ascii inp)
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"));
      order_ db u


and graph_ db u =
      print_database db u;
      print_endline " \n";
  print_string "Type exit to exit:  ";
    match read_line () with
      | exception End_of_file ->
      print_endline redo;
      print_string u;
      order_ db u
  | inp -> if "exit" = String.lowercase_ascii inp then account db u

and account (db : Database.t) (u : string) =
  print_endline " \n";
  print_string "Current User: ";
  ANSITerminal.print_string [ ANSITerminal.blue ] u;
  print_endline " \n";
  print_string "Your USD balance is: $";
  ANSITerminal.print_string [ ANSITerminal.blue ] (string_of_int (user_balance db u "usd"));
  print_endline " \n";
  print_string "Your BRB balance is: $";
  ANSITerminal.print_string [ ANSITerminal.blue ] (string_of_int (user_balance db u "brb"));
  print_endline " \n";
  ANSITerminal.print_string [ ANSITerminal.red ] "\n Pleae select a menu";
  
  print_string
    "\ \n\
    \    - Deposit Currency\n\n\
    \    - Withdraw Currency\n\n\
    \    - Order\n\n\
    \    - Graph\n\n\
    \    - Home\n\n\
    \    - Quit\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      print_string u;
      account db u
  | t -> begin
      match parse t with
      | exception Malformed -> account db u
      | exception Empty -> account db u
      | Deposit x -> deposit_ db u
      | Withdraw x -> withdraw_ db u
      | Order -> order_ db u
      | Graph -> graph_ db u
      | Home -> home db u
      | Quit -> quit db u
      | _ -> account db u
    end

and login f =
  print_endline "\nWould you like to login or create a new user?\n";
  ANSITerminal.print_string [ ANSITerminal.black ] "Please enter\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] " - login username";
  ANSITerminal.print_string [ ANSITerminal.black ] " to login\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] " - new_user username";
  ANSITerminal.print_string [ ANSITerminal.black ] " to create a new user\n";

  (* print_endline "Type login [username] to login or new_user [username]
     \n"; *)
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline redo;
      login f
  | t -> begin
      match parse t with
      | exception Malformed ->
          print_endline redo;
          login f
      | exception Empty ->
          print_endline redo;
          login f
      | Login x ->
          if List.mem (List.hd x) (users f) then account f (List.hd x)
          else print_endline "-------------------------------------------";
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Entered username does not exist! Try again!\n";
          login f
      | New_User x -> account (new_user f (List.hd x)) (List.hd x)
      | _ ->
          print_endline redo;
          login f
    end

and create_da_file db =
  new_db_file (db_name db) "";
  login db

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the trading platform.\n";
  ANSITerminal.print_string [ ANSITerminal.black ] "\nPlease enter \n -";
  ANSITerminal.print_string [ ANSITerminal.blue ] " database name ";
  ANSITerminal.print_string [ ANSITerminal.black ] "to load database";
  ANSITerminal.print_string [ ANSITerminal.blue ] " name\n - ";
  ANSITerminal.print_string [ ANSITerminal.blue ] "new_database name";
  ANSITerminal.print_string [ ANSITerminal.black ]
    " to create new database with new";
  ANSITerminal.print_string [ ANSITerminal.blue ] " name\n";

  (* print_endline "Please enter [database [database name]] to load a database
     \n\n\ \ or [new_database database] to create a new one"; *)
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | t -> begin
      match parse t with
      | exception Malformed ->
          print_endline redo;
          main ()
      | exception Empty ->
          print_endline redo;
          main ()
      | NewDatabase x -> create_da_file (new_database (List.hd x))
      | Database x ->
          Yojson.Basic.from_file (data_dir_prefix ^ List.hd x ^ ".json")
          |> Yojson.Basic.to_string |> from_json |> login
      | _ ->
          print_endline redo;
          main ()
    end

(* Execute the (from_json (Yojson.Basic.from_file (data_dir_prefix ^ str ^
   ".json"))) engine. *)

let () = main ()


(*
(* Defines the type of a command, which represents an action that can be taken with the database.
   The possible commands are:
   - NewDatabase of object_phrase: creates a new database with the given object phrase
   - Database of object_phrase: opens the database with the given object phrase
   - Login of object_phrase: logs in to the database with the given object phrase
   - New_User of object_phrase: creates a new user in the database with the given object phrase
   - Home: navigates to the home menu
   - Account: navigates to the account menu
   - Admin: navigates to the admin menu
   - Deposit of object_phrase: deposits the given amount into the account with the given object phrase
   - Withdraw of object_phrase: withdraws the given amount from the account with the given object phrase
   - View_Bal: views the balance of the current user's account
   - View_Users: views the list of users in the database
   - Quit: quits the program
*)
type command =
  | NewDatabase of object_phrase
  | Database of object_phrase
  | Login of object_phrase
  | New_User of object_phrase
  | Home
  | Account
  | Admin
  | Deposit of object_phrase
  | Withdraw of object_phrase
  | View_Bal
  | View_Users
  | Quit

(* Defines an exception for when a command is empty. *)
exception Empty
(* Defines an exception for when a command is malformed. *)
exception Malformed

(* Helper function for parsing a single word command. *)
let com z =
  match String.lowercase_ascii z with
  | "" -> raise Empty
  | "quit" -> Quit
  | "home" -> Home
  | "account" -> Account
  | "admin" -> Admin
  | "view_balance" -> View_Bal
  | "view_users" -> View_Users
  | _ -> raise Malformed

(* Helper function for parsing a multi-word command. *)
let coms z sl =
  let len = List.length sl in
  match String.lowercase_ascii z with
  | "deposit" -> Deposit sl
  | "withdraw" -> Withdraw sl
  | _ when len > 1 -> raise Malformed
  | "" -> raise Empty
  | "new_database" -> NewDatabase sl
  | "database" -> Database sl
  | "login" -> Login sl
  | "new_user" -> New_User sl
  | _ -> raise Malformed

(* Defines a string to be displayed when an input is invalid. *)
let redo = "please try again thank you!"

(* Splits a string into a list of words, filtering out empty strings. *)
let string_split s = List.filter (fun x -> x <> "") (String.split_on_char ' ' s)

(* Parses a list of words into a command. *)
let parser s =
  match s with
  | [] -> raise Empty
  | [ h ] -> com h
  | h :: t -> coms h t

Creates a new json file in the `data` directory with the given filename and json string.
   The file is saved as `data/json*)
