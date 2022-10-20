type t
(**the abstract type of an object representing a bid/ask database with users and
   orders. Users have usernames and a balance in cents of BRB and USD*)

val new_database : string -> t
(**[new_database] is a database of type t with db_name name with no users or
   orders*)

val from_json : string -> t
(**takes in a json string, creates abstract db type t*)

val db_name : t -> string
(**[db_name db] is the database's name*)

val new_user : t -> string -> t
(**[new_user db name] is the database b with a new user with username name added*)

val users : t -> string list
(**[users db] is a list of the usernames of all the users in db*)

val deposit : t -> string -> string -> int -> t
(**[deposit db name curr amt] is db with user name having amt more of currency
   curr Requires: name is valid name of a user in db, curr is either USD or BRB*)

val withdraw : t -> string -> string -> int -> t
(**[withdraw db name curr amt] is db with user name having amt less of currency
   curr Requires: name is valid name of a user in db, curr is either usd or brb*)

val user_balance : t -> string -> string -> int
(** Example: [user_balance db tony brb] is tony's balance of brbs (in brb cents) *)

val to_json : t -> string
(**[to_json db] is a json in string form that represents db.t*)