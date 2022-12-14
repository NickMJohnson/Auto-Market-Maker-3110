open Database

(* Define the function that takes a list of orders and prints a graph *)
let print_order_graph (buy_orders : order list) (sell_orders : order list) =
  (* Calculate the maximum rate and amount from the orders *)
  let max_rate_b =
    List.fold_left
      (fun acc order -> max acc (int_of_float (order.rate *. 100.) / 5))
      0 buy_orders
  in
  let max_amount_b =
    List.fold_left (fun acc order -> max acc (order.amount / 5)) 0 buy_orders
  in
  let max_rate_s =
    List.fold_left
      (fun acc order -> max acc (int_of_float (order.rate *. 100.) / 5))
      0 sell_orders
  in
  let max_amount_s =
    List.fold_left (fun acc order -> max acc (order.amount / 5)) 0 sell_orders
  in
  let min_rate_s =
    List.fold_left
      (fun acc order -> min acc (int_of_float (order.rate *. 100.) / 5))
      max_rate_s sell_orders
  in
  let max_rate = max max_rate_b max_rate_s in
  let max_amount = max max_amount_b max_amount_s in

  (* Create a grid of characters with the given dimensions *)
  let grid = Array.make_matrix (max_rate + 1) (max_amount + 1) "   " in

  (* Iterate over the orders and fill in the grid with '*' characters at the
     corresponding coordinates *)
  List.iter
    (fun order ->
      for x = 0 to order.amount / 5 do
        grid.(int_of_float (order.rate *. 100.) / 5).(x) <- "BB "
      done)
    buy_orders;
  List.iter
    (fun order ->
      for x = 0 to order.amount / 5 do
        grid.(int_of_float (order.rate *. 100.) / 5).(x) <- "SS "
      done)
    sell_orders;

  (* Print the x axis labels *)

  (* Print the grid to the terminal *)
  for y = max_amount downto 0 do
    (* Print the y axis labels *)
    print_int (y * 5);
    if y * 5 < 10 then print_char ' ';
    print_char ' ';
    print_char '|';
    print_char ' ';

    for x = 0 to max_rate do
      let symbol = grid.(x).(y) in
      if symbol = "BB " then
        ANSITerminal.print_string [ ANSITerminal.green ] symbol
      else if symbol = "SS " then
        ANSITerminal.print_string [ ANSITerminal.red ] symbol
      else ANSITerminal.print_string [ ANSITerminal.blue ] symbol
    done;
    print_newline ()
  done;
  print_char ' ';
  print_char ' ';
  print_char ' ';
  print_char ' ';
  print_char ' ';

  for x = 0 to 20 do
    ANSITerminal.print_string [ ANSITerminal.black ] "___"
  done;
  print_newline ();
  print_char ' ';
  print_char ' ';
  print_char ' ';
  print_char ' ';
  print_char ' ';

  for x = 0 to 20 do
    if x * 5 < 10 then print_int 0;
    print_int (x * 5);
    print_char ' '
  done;
  print_newline ();
  print_string "Highest bid: 0.";
  print_int (5 * max_rate_b);
  print_string " Usd/Brb";
  print_newline ();
  print_string "Lowest ask: 0.";
  print_int (5 * min_rate_s);
  print_string " Usd/Brb";
  print_newline ();
  print_string "Market Price: 0.";
  print_int ((min_rate_s + max_rate_b) / 2 * 5);
  print_string " Usd/Brb"

(*let orders = [ { user = "Alice";rate = 10; amount = 10 }; { user = "Bob"; rate
  = 10; amount = 15 }; { user = "Alice"; rate = 30; amount = 20 }; { user =
  "Charlie"; rate = 40; amount = 10 }; { user = "Alice";rate = 40; amount = 20
  }; { user = "Bob"; rate = 45; amount = 35 }; { user = "Alice"; rate = 47;
  amount = 40 }; { user = "Charlie"; rate = 50; amount = 50 }; { user =
  "Alice";rate = 51; amount = 70 }; { user = "Bob"; rate = 10; amount = 75 }; {
  user = "Alice"; rate = 20; amount = 80 }; { user = "Charlie"; rate = 00;
  amount = 50 } ] let sell_orders = [ { user = "Alice";rate = 60; amount = 10 };
  { user = "Bob"; rate = 65; amount = 15 }; { user = "Alice"; rate = 70; amount
  = 20 }; { user = "Charlie"; rate = 80; amount = 10 }; { user = "Alice";rate =
  90; amount = 20 }; { user = "Bob"; rate = 95; amount = 35 }; { user = "Alice";
  rate = 100.; amount = 40 }; { user = "Alice"; rate = 70; amount = 40 } ]*)

let print_database db u =
  print_order_graph db.orders.buy_orders db.orders.sell_orders
