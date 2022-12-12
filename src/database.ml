open Yojson.Basic.Util

type user = {
  name : string;
  usd : int;
  brb : int;
}
[@@deriving yojson]

and t = {
  db_name : string;
  users : user list;
  orders : order_book;
}
[@@deriving yojson]

and order = {
  user : string;
  amount : int;
  rate : float;
}
[@@deriving yojson]

and order_book = {
  buy_orders : order list;
  sell_orders : order list;
}
[@@deriving yojson]

let from_json (json : string) : t =
  json |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok

let empty_book = { buy_orders = []; sell_orders = [] }

let new_database (name : string) : t =
  { users = []; db_name = name; orders = empty_book }
(*TODO: Make this better. Like it could use from_json on an empty db file we
  keep updated*)

let db_name db = db.db_name

(*[new_user db name] is the database b with a new user with username name
  added *)
let new_user db nm =
  { db with users = { name = nm; usd = 0; brb = 0 } :: db.users }

(*[users db] is a list of the usernames of all the users in db*)
let users db = List.map (fun x -> x.name) db.users

(*[deposit db name curr amt] is db with user name having amt more of currency
  curr Requires: name is valid name of a user in db, curr is either USD or BRB*)
(*TODO: Integrate and fix. Can use library functions to make much better prob.*)
let deposit db name curr amt =
  let cap = String.uppercase_ascii curr in
  let rec find_user users prev =
    match users with
    | [] -> db.users
    | h :: s ->
        if h.name = name then
          if cap = "USD" then prev @ [ { h with usd = h.usd + amt } ] @ s
          else prev @ [ { h with brb = h.brb + amt } ] @ s
        else find_user s (prev @ [ h ])
  in
  { db with users = find_user db.users [] }

(*[withdraw db name curr amt] is db with user name having amt less of currency
  curr Requires: name is valid name of a user in db, curr is either usd or brb*)
let withdraw db name curr amt = deposit db name curr (-amt)

(* Example: [user_balance db name curr] is tony's balance of curr (in brb
   cents) *)
let user_balance db name curr =
  let cap = String.uppercase_ascii curr in
  let rec find_user users =
    match users with
    | [] -> 0
    | h :: s ->
        if h.name = name then if cap = "USD" then h.usd else h.brb
        else find_user s
  in
  find_user db.users

let get_user db name =
  match db.users with
  | [] -> failwith "invalid name"
  | h :: t -> h

let ( *> ) i f = i |> float_of_int |> ( *. ) f |> int_of_float

let sort_orders db =
  {
    db with
    orders =
      {
        buy_orders =
          List.sort
            (fun o1 o2 -> Float.compare o2.rate o1.rate)
            db.orders.buy_orders;
        (*buy orders sorted largest rate first*)
        sell_orders =
          List.sort
            (fun o1 o2 -> Float.compare o1.rate o2.rate)
            db.orders.sell_orders
          (*buy orders sorted smallest rate first*);
      };
  }

let rec buy_order db name amt (rate : float) =
  let charge_orderer = withdraw db name "usd" (amt *> rate) in
  charge_orderer
  |> buy_order_filler { user = name; amount = amt; rate }
  |> sort_orders

and update_user_balance name usd_amt ul =
  match ul with
  | [] -> failwith "user has an order but does not exist"
  | h :: t ->
      if h.name = name then { h with usd = h.usd + usd_amt } :: t
      else h :: update_user_balance name usd_amt t

and buy_order_filler order db =
  if order.amount <= 0 then db
  else
    match db.orders.sell_orders with
    | [] ->
        {
          db with
          orders = { db.orders with buy_orders = order :: db.orders.buy_orders };
        }
    | h :: t when order.rate < h.rate ->
        {
          db with
          orders = { db.orders with buy_orders = order :: db.orders.buy_orders };
        }
    | h :: t ->
        if h.amount <= order.amount then
          let db_no_h =
            {
              db with
              orders = { db.orders with sell_orders = t };
              users = update_user_balance h.user (h.amount *> h.rate) db.users;
            }
          in
          let updated_order = { order with amount = order.amount - h.amount } in
          buy_order_filler updated_order db_no_h
        else
          (*If the smallest sell order is enough to fulfill our order*)
          let db_smaller_h =
            {
              db with
              orders =
                {
                  db.orders with
                  sell_orders = { h with amount = h.amount - order.amount } :: t;
                };
              users =
                update_user_balance h.user (order.amount *> h.rate) db.users;
            }
          in
          let updated_order =
            { order with amount = order.amount - order.amount }
          in
          buy_order_filler updated_order db_smaller_h

let rec sell_order db name amt (rate : float) =
  let charge_orderer = withdraw db name "brb" amt in
  {
    charge_orderer with
    orders =
      {
        db.orders with
        sell_orders =
          { user = name; amount = amt; rate } :: db.orders.sell_orders;
      };
  }
  |> sort_orders

(*let rec sell_order db name amt (rate : float) = let charge_orderer = withdraw
  db name "brb" (amt) in charge_orderer |> buy_order_filler { user = name;
  amount = amt; rate } |> sort_orders

  and update_user_balance name usd_amt ul = match ul with | [] -> failwith "user
  has an order but does not exist" | h :: t -> if h.name = name then { h with
  usd = h.usd + usd_amt } :: t else h :: update_user_balance name usd_amt t

  and sell_order_filler order db = if order.amount <= 0 then db else match
  db.orders.sell_orders with | [] -> { db with orders = { db.orders with
  buy_orders = order :: db.orders.buy_orders }; } | h :: t when order.rate <
  h.rate -> { db with orders = { db.orders with buy_orders = order ::
  db.orders.buy_orders }; } | h :: t -> if h.amount <= order.amount then let
  db_no_h = { db with orders = { db.orders with sell_orders = t }; users =
  update_user_balance h.user (h.amount *> h.rate) db.users; } in let
  updated_order = { order with amount = order.amount - h.amount } in
  buy_order_filler updated_order db_no_h else (*If the smallest sell order is
  enough to fulfill our order*) let db_smaller_h = { db with orders = {
  db.orders with sell_orders = { h with amount = h.amount - order.amount } :: t;
  }; users = update_user_balance h.user (order.amount *> h.rate) db.users; } in
  let updated_order = { order with amount = order.amount - order.amount } in
  buy_order_filler updated_order db_smaller_h*)

(**[to_json db] is a json in string form that represents db*)
let to_json (db : t) : string = db |> to_yojson |> Yojson.Safe.to_string
