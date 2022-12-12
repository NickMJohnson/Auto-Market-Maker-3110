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

let deposit name curr amt db =
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

let withdraw name curr amt db = deposit name curr (-amt) db

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

let rec buy_order name amt (rate : float) db =
  db |> buy_order_filler { user = name; amount = amt; rate } |> sort_orders

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
          let remove_head =
            { db with orders = { db.orders with sell_orders = t } }
            |> deposit h.user "usd" (h.amount *> h.rate)
            |> withdraw order.user "usd" (h.amount *> h.rate)
            |> deposit order.user "brb" h.amount
            |> withdraw h.user "brb" h.amount
          in

          let updated_order = { order with amount = order.amount - h.amount } in
          buy_order_filler updated_order remove_head
        else
          (*If the smallest sell order is enough to fulfill our order*)
          let smaller_head =
            {
              db with
              orders =
                {
                  db.orders with
                  sell_orders = { h with amount = h.amount - order.amount } :: t;
                };
            }
            |> deposit h.user "usd" (order.amount *> h.rate)
            |> withdraw order.user "usd" (order.amount *> h.rate)
            |> deposit order.user "brb" order.amount
            |> withdraw h.user "brb" order.amount
          in

          let updated_order = { order with amount = 0 } in
          buy_order_filler updated_order smaller_head

let rec sell_order name amt (rate : float) db =
  db |> sell_order_filler { user = name; amount = amt; rate } |> sort_orders

and sell_order_filler order db =
  if order.amount <= 0 then db
  else
    match db.orders.buy_orders with
    | [] ->
        {
          db with
          orders =
            { db.orders with sell_orders = order :: db.orders.sell_orders };
        }
    | h :: t when order.rate > h.rate ->
        {
          db with
          orders =
            { db.orders with sell_orders = order :: db.orders.sell_orders };
        }
    | h :: t ->
        if h.amount <= order.amount then
          let remove_head =
            { db with orders = { db.orders with buy_orders = t } }
            |> deposit h.user "brb" h.amount
            |> withdraw order.user "brb" h.amount
            |> deposit order.user "usd" (h.amount *> h.rate)
            |> withdraw h.user "usd" (h.amount *> h.rate)
          in
          let updated_order = { order with amount = order.amount - h.amount } in
          sell_order_filler updated_order remove_head
        else
          (*If the smallest sell order is enough to fulfill our order*)
          let smaller_head =
            {
              db with
              orders =
                {
                  db.orders with
                  buy_orders = { h with amount = h.amount - order.amount } :: t;
                };
            }
            |> deposit h.user "brb" order.amount
            |> withdraw order.user "brb" order.amount
            |> deposit order.user "usd" (order.amount *> h.rate)
            |> withdraw h.user "usd" (order.amount *> h.rate)
          in
          let updated_order = { order with amount = 0 } in
          sell_order_filler updated_order smaller_head
(* let rec sell_order name amt (rate : float) db = let charge_orderer = withdraw
   name "brb" amt db in { charge_orderer with orders = { db.orders with
   sell_orders = { user = name; amount = amt; rate } :: db.orders.sell_orders;
   }; } |> sort_orders *)

(**[to_json db] is a json in string form that represents db*)
let to_json (db : t) : string = db |> to_yojson |> Yojson.Safe.to_string
