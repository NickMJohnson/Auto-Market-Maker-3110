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
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)


val parse : string -> command