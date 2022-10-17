open Database
open Yojson.Basic.Util

type user = {
  name : string;
  usd : int;
  brb : int;
}

let user_of_json j =
  {
    name = j |> member "name" |> to_string;
    usd = j |> member "usd" |> to_int;
    brb = j |> member "brb" |> to_int;
  }

type t = { users : user list }

let from_json (str : string) : t =
  let j = Yojson.Basic.from_string str in
  { users = j |> member "users" |> to_list |> List.map user_of_json }

let new_database : t = { users = [] }
(*TODO: Make this better. Like it could use from_json on an empty db file we
  keep updated*)
