open Yojson.Basic.Util

type user = {
  name : string;
  usd : int;
  brb : int;
}
[@@deriving yojson]

type t = {
  db_name : string;
  users : user list;
}
[@@deriving yojson]

let from_json (json : string) : t =
  json |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok

let new_database (name : string) : t = { users = []; db_name = name }
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

(**[to_json db] is a json in string form that represents db*)
let to_json (db : t) : string = db |> to_yojson |> Yojson.Safe.to_string
