open Database

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

(* Create New File under data folder https://ocaml.org/docs/file-manipulation *)
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
  | h :: t -> h ^ " , " ^ print_users t

let rec home (db : Database.t) (u : string) =
  print_string
    "\n\n \n    Pleae select a menu \n\n    - Admin\n\n    - Account\n";
  match read_line () with
  | exception End_of_file -> print_endline(redo); home db u
  | inp ->
      if "admin" = String.lowercase_ascii inp then admin db u
      else if "account" = String.lowercase_ascii inp then account db u
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n")

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
  | exception End_of_file -> print_endline(redo); admin db u
  | inp ->
      if "view users" = String.lowercase_ascii inp then view_users db u
      else if "home" = String.lowercase_ascii inp then home db u
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n")

and quit db u= 
print_endline("Goodbye ");
print_string u;
update_json db

and withdraw__ db u curr =
  print_endline "How much would you like to withdraw:\n";
  print_endline "Type an amount \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_endline(redo); print_string u; withdraw__ db u curr
  | inp -> account (withdraw db u curr (int_of_string inp)) u

and withdraw_ db u =
  print_endline "What would you like to withdrawl?:\n";
  print_endline "Type brb to withdraw brb or usd to withdraw usd \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_endline(redo); print_string u; withdraw_ db u
  | inp -> if "brb" = String.lowercase_ascii inp then
        withdraw__ db u (String.lowercase_ascii inp)
      else if "usd" = String.lowercase_ascii inp then
        withdraw__ db u (String.lowercase_ascii inp)
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n")

and deposit__ db u curr =
  print_endline "How much would you like to deposit:\n";
  print_endline "Type an amount \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_endline(redo); print_string u; deposit__ db u curr
  | inp -> account (deposit db u curr (int_of_string inp)) u

and deposit_ db u =
  print_endline "What would you like to deposit?:\n";
  print_endline "Type brb to deposit brb or usd to deposit usd \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_endline(redo); print_string u; deposit_ db u
  | inp ->
      if "brb" = String.lowercase_ascii inp then
        deposit__ db u (String.lowercase_ascii inp)
      else if "usd" = String.lowercase_ascii inp then
        deposit__ db u (String.lowercase_ascii inp)
      else ANSITerminal.(print_string [ blue; Bold ] "\nInvalid input. \n"); deposit_ db u

and account (db : Database.t) (u : string) =
  print_endline "Current User is:\n";
  print_endline u;
  print_endline "Your BRB balance is:\n\n";
  print_int (user_balance db u "usd");
  print_endline " \n";
  print_endline "Your BRB balance is:\n\n";
  print_int (user_balance db u "brb");
  print_string
    "\n\n\
    \ \n\
    \    Pleae select a menu \n\n\
    \    - Deposit Currency\n\n\
    \    - Withdraw Currency\n\n\
    \    - Home\n\n\
    \    - Quit\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_endline(redo); print_string u; account db u
  | t -> begin
      match parse t with
      | exception Malformed -> account db u
      | exception Empty -> account db u
      | Deposit x -> deposit_ db u
      | Withdraw x -> withdraw_ db u
      | Home -> home db u
      | Quit -> quit db u
      | _ -> account db u
    end

and login f =
  print_endline "Would you like to login or create a new user\n";
  print_endline "Type login [username] to login or new_user [username] \n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_endline(redo); login f
  | t -> begin
      match parse t with
      | exception Malformed -> print_endline(redo); login f
      | exception Empty -> print_endline(redo); login f 
      | Login x -> account f (List.hd x)
      | New_User x -> account (new_user f (List.hd x)) (List.hd x)
      | _ -> print_endline(redo); login f
    end

and create_da_file db  = new_db_file (db_name db) ""; login db

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the trading platform.\n";
  print_endline
    "Please enter [database [database name]] to load a database \n\n\
    \  or [new_database database] to create a new one";
  print_string "> \n";
  match read_line () with
  | exception End_of_file -> ()
  | t -> begin
      match parse t with
      | exception Malformed -> print_endline(redo); main ()
      | exception Empty -> print_endline(redo); main ()
      | NewDatabase x -> create_da_file (new_database (List.hd x))
      | Database x ->
          Yojson.Basic.from_file (data_dir_prefix ^ List.hd x ^ ".json")
          |> Yojson.Basic.to_string |> from_json |> login
      | _ -> print_endline(redo); main ()
    end

(* Execute the (from_json (Yojson.Basic.from_file (data_dir_prefix ^ str ^
   ".json"))) engine. *)

let () = main ()
