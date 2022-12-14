(** Trading platform between USD and BRB implemented through a json database. *)

type t = {
  db_name : string;
  users : user list;
  orders : order_book;
}
(**The type of an object representing a bid/ask database for USD and BRB with
   users and orders. [db_name] is the name of the database.*)

and user = {
  name : string;
  usd : int;
  brb : int;
}
(**The type a databse user. A user has a name, and a balance of USD and BRB. *)

and order_book = {
  buy_orders : order list;
  sell_orders : order list;
}
(**The type of an order book. An order book has a list of buy orders and a list
   of sell orders. An order in buy_orders is a buy order, and in sell_orders is
   a sell order*)

and order = {
  user : string;
  amount : int;
  rate : float;
}
(**The type an order. A user has a name, an amount requested to buy or sell (in
   BRB) and a rate (in USD per BRB)*)

val new_database : string -> t
(**[new_database db_name] is a database of type t with name db_name with no
   users or orders. Requires: db_name must be nonempty *)

val from_json : string -> t
(**[from_json json] is the databse represented by [json]. Requires: json is
   valid json representing a database*)

val db_name : t -> string
(**[db_name db] is the database's name*)

val new_user : t -> string -> t
(**[new_user db name] is the database b with a new user with username name added*)

val users : t -> string list
(**[users db] is a list of the usernames of all the users in db*)

val deposit : string -> string -> int -> t -> t
(**[deposit name curr amt db] is db with user name having amt more of currecny
   curr. Requires: name is valid name of a user in db, curr is either "USD" or
   "BRB"*)

val withdraw : string -> string -> int -> t -> t
(**[withdraw name curr amt db] is db with user name having amt less of currency
   curr. Requires: name is valid name of a user in db, curr is either "usd" or
   "brb"*)

val user_balance : t -> string -> string -> int
(** [user_balance db name curr] is name's balance of curr. Requires: curr is
    either "usd" or "brb" *)

val buy_order : string -> int -> float -> t -> t
(**[buy_order user amt rate db] is db with buy order from user looking to buy
   amt BRBs with exchange rate [rate]. The order is filled as much as possible
   by any sell orders with a rate lower than the order's rate. When an order is
   filled, the appropriate amout of each currency is transferered between the
   two trading accounts. Int Requires: amt is an integer > 0 , rate is an float
   > 0 and <= 1, name is a valid user*)

val sell_order : string -> int -> float -> t -> t
(**[sell_order db user amt rate] is db looking to sell amt BRBs with exchange
   rate [rate]. The order is filled as much as possible by any buy orders with a
   rate higher than the order's rate. When an order is filled, the appropriate
   amout of each currency is transferered between the two trading accounts.
   Requires: amt is an integer > 0 , rate is an integer > 0 and <= 100 *)

val to_json : t -> string
(**[to_json db] is a string json that represents db.t*)