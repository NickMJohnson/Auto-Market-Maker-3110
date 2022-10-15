type t

val new_database : t

val from_json : string -> t
(**takes in a json string, creates abstract db type t*)

val new_user : t -> string -> t
(**[new_user db name] is the database b with a new user with username name added*)

val users : t -> string list
val deposit : t -> string -> string -> int -> t
val withdraw : t -> string -> string -> int -> t

val user_balance : t -> string -> string -> int
(** Example: [user_balance db tony brb] is tony's balance of brbs (in brb cents) *)

val save_to_json : t -> string
(**[save_to_json db] is a json in string form that represents db.t*)