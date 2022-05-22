open ANSITerminal
open OFinance
open Spread 


let blackscholes_date  lst = 
  let time = Blackscholes.create_time 0 0 0 0  in 
  Blackscholes.create_date (int_of_string (List.nth lst 0)) ( int_of_string (List.nth lst 1)) (int_of_string (List.nth lst 2)) time

let spread_main () = print_endline
  "Please enter the name of the desired spread.";
  match read_line () with
  | exception End_of_file -> ()
  | name -> let spread = make_spread name in
  print_endline "Please enter the current date as mm/dd/2022.";
  match read_line () with
  | exception End_of_file -> ()
  | date ->  let date_lst =  String.split_on_char '/' date in 
  let date = blackscholes_date date_lst in
  print_endline "Please enter the current price of the underlying (float).";
  match read_line () with
  | exception End_of_file -> ()
  | price -> print_endline "Price of spread: " ;
  print_float (price_spread spread (float_of_string price) date); 
  print_endline "\n"
