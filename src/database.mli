(*TODO: Order book {buy order list; sell order list} {name amt rate time} *)

type user = {
  name : string;
  usd : int;
  brb : int;
}

and t = {
  db_name : string;
  users : user list;
  orders : order_book;
}

and order = {
  user : string;
  amount : int;
  rate : float;
}

and order_book = {
  buy_orders : order list;
  sell_orders : order list;
}
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

val buy_order : t -> string -> int -> float -> t
(**[buy_order db user
   amt rate] is (db, remains) looking to buy amt BRBs with
   exchange rate [rate]. User's USD balance is lowered by amt * rate, and the
   order is added to their account. Int Requires: amt is an integer > 0 , rate
   is an float > 0 and <= 1, name is a valid user

   val sell_order : t -> string -> int -> int -> t * int
   (**[sell_order db user
   amt rate] is db with user putting amt BRB into the
   order pool, looking for USDs with exchange rate [rate]. User's B RB balance
   is lowered by amt, and the order is added to their account. Requires: amt is
   an integer > 0 , rate is an integer > 0 and <= 100 *) *)

(* val account_status : t -> string -> user *)
(**[acccount_status db user] returns the user record with name user in db.
   Requires: name is a valid user*)

val to_json : t -> string
(**[to_json db] is a json in string form that represents db.t*)