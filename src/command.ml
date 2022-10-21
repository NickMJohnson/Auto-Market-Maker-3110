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

let parse s = parser (string_split s)
