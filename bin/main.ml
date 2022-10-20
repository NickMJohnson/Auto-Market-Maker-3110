open Database 

type object_phrase = string list

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

let coms z sl= 
  match String.lowercase_ascii z with 
  | "" -> raise Empty
  | "new_database" -> NewDatabase sl
  | "database" -> Database sl
  | "login" -> Login sl
  | "new user" -> New_User sl
  | "deposit" -> Deposit sl
  | "witdraw" -> Withdraw sl
  | _ -> raise Malformed

let string_split s = List.filter (fun x -> x <> "") (String.split_on_char ' ' s)
let parser s = 
  match s with
  |[] -> raise (Empty)
  | h::[] -> com h
  | h::t -> coms h t 

let home db u= 
  print_string
  "\n\n 
    Pleae select a menu \n
    - Admin\n
    - Account\n";
  match read_line () with
  |exception End_of_file -> ()
  |inp -> if "admin" = (String.lowercase_ascii inp) then admin db u
  else if "account" = (String.lowercase_ascii inp) then account db u
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));

let admin db u = 
  print_string
  "\n\n 
    Pleae select a menu \n
    - View Users\n
    - Home\n";
  match read_line () with
  |exception End_of_file -> ()
  |inp -> if "view users" = (String.lowercase_ascii inp) then view_users db u
  else if "home" = (String.lowercase_ascii inp) then home db u
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));

let deposit__ db u curr = 
print_endline "How much would you like to deposit:\n";
match read_line () with
  |exception End_of_file -> ()
  |inp -> account (deposit db u curr (int_of_string inp)) u 

let deposit_ db u = 
  print_endline "What would you like to deposit?:\n";
  match read_line () with
    |exception End_of_file -> ()
    |inp -> if "brb" = (String.lowercase_ascii inp) then deposit__ db u (String.lowercase_ascii inp)
    else if "usd" = (String.lowercase_ascii inp) then deposit__ db u (String.lowercase_ascii inp)
    else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));

let view_bal db u = 
  print_endline "Your BRB balance is:\n";
  print_int(user_balance db u "usd");
  print_endline "Your BRB balance is:\n";
  print_int(user_balance db u "brb");
  account db u

let user_l f = 
  print_endline "What is your name\n";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |user -> account f user


let new_user1 f = 
  ANSITerminal.print_string [ ANSITerminal.red ]
  print_endline "What is your name\n";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |user -> account (new_user f user) user


Let login f = 
  ANSITerminal.print_string [ ANSITerminal.red ]
  print_endline "Would you like to login or create a new user\n";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |log -> if "login" = (String.lowercase_ascii log) then user_l f
  else if "new user" = (String.lowercase_ascii log) then new_user1 f
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));


let data_dir_prefix = "data" ^ Filename.dir_sep

let account (db : database.t) (u : string) = 
  ANSITerminal.print_string []
  print_endline "Current User is:\n";
  print_string u;
  print_string
  "\n\n 
    Pleae select a menu \n
    - View Balance\n
    - Deposit\n
    - Withdraw\n
    - Home\n
    - Quit";
  match read_line () with
  |exception End_of_file -> ()
  |inp -> if "view balance" = (String.lowercase_ascii inp) then view_bal db u
  else if "deposit" = (String.lowercase_ascii inp) then deposit_ db u
  else if "withdraw" = (String.lowercase_ascii inp) then withdraw db u
  else if "home" = (String.lowercase_ascii inp) then home db u
  else if "quit" = (String.lowercase_ascii inp) then quit db
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the trading platform.\n";
  print_endline "Please enter load database to load a database \n
  or new database to create a new one";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> if "new database" = (String.lowercase_ascii str) then login (new_database)
  else login (from_json (Yojson.Basic.from_file (data_dir_prefix ^ str ^ ".json")));


(* Execute the engine. *)
let () = main ()

