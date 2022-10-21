
open Database
let rec home (db : Database.t) (u : string)= 
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



and admin db u =
  print_string
  "\n\n 
    Pleae select a menu \n
    - View Users\n
    - Home\n";
  match read_line () with
  |exception End_of_file -> ()
  |inp -> if "view users" = (String.lowercase_ascii inp) then home db u
  else if "home" = (String.lowercase_ascii inp) then home db u
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));


and account (db : Database.t) (u : string) = 
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
  else if "quit" = (String.lowercase_ascii inp) then quit db u
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));

and user_l f = 
  print_endline "What is your name\n";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |user -> account f user

and quit db u = home db u
and withdraw db u = home db u

and new_user1 f = 
  print_endline "What is your name\n";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |user -> account (new_user f user) user


and view_bal db u = 
  print_endline "Your BRB balance is:\n";
  print_int(user_balance db u "usd");
  print_endline "Your BRB balance is:\n";
  print_int(user_balance db u "brb");
  account db u

and deposit__ db u curr = 
  print_endline "How much would you like to deposit:\n";
  match read_line () with
  |exception End_of_file -> ()
  |inp -> account (deposit db u curr (int_of_string inp)) u 

and deposit_ db u = 
  print_endline "What would you like to deposit?:\n";
  match read_line () with
  |exception End_of_file -> ()
  |inp -> if "brb" = (String.lowercase_ascii inp) then deposit__ db u (String.lowercase_ascii inp)
  else if "usd" = (String.lowercase_ascii inp) then deposit__ db u (String.lowercase_ascii inp)
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));



and login f = 
  print_endline "Would you like to login or create a new user\n";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |log -> if "login" = (String.lowercase_ascii log) then user_l f
  else if "new user" = (String.lowercase_ascii log) then new_user1 f
  else ANSITerminal.(print_string [blue; Bold] ("\nInvalid input. \n"));


(** [main ()] prompts for the game to play, then starts it. *)
and main () = 
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the trading platform.\n";
  print_endline "Please enter [load database [database name]] to load a database \n
  or [new database] to create a new one";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> if "new database" = (String.lowercase_ascii str) then (login (new_database "new")) 
  else print_string("no")


(* Execute the (from_json (Yojson.Basic.from_file (data_dir_prefix ^ str ^ ".json"))) engine. *)

let () = main ()
