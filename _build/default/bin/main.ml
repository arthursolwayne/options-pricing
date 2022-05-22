open ANSITerminal
open OFinance

(* open Blackscholes
open Spread *)

let blackscholes_date  lst = 
  let time = Blackscholes.create_time 0 0 0 0  in 
  Blackscholes.create_date (int_of_string (List.nth lst 0)) ( int_of_string (List.nth lst 1)) (int_of_string (List.nth lst 2)) time

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Cornell Quant Funds Blackscholes options pricing model.\n";
  print_endline
    "Please enter the current price of stock (must be float, write '40$' as '40.').";
  match read_line () with
  | exception End_of_file -> ()
  | stock_price -> print_endline
  "Please enter the strike price of the option (must be float).";
  match read_line () with
    | exception End_of_file -> ()
    | strike_price -> print_endline
    "Please enter the expiration date as (m/d/2022).";
    match read_line () with
      | exception End_of_file -> ()
      | date -> print_endline
      "Please enter the risk free interest rate (Usually 30-year US Treasury bond yield [.0248]).";
      match read_line () with
        | exception End_of_file -> ()
        | risk_free_rate -> print_endline
        ("Please enter the implied volatility (annualized standard deviation of asset returns)\n" ^
        "or: 0.1 -> 0.2 : not volatile, 0.2 -> 0.4 : fairly volatile, 0.4+ : highly volatile.");
        match read_line () with
        | exception End_of_file -> ()
        | implied_volatility -> print_endline
            "Please enter the current date as (m/d/2022).";
            match read_line () with
              | exception End_of_file -> ()
              | current_date -> print_endline
              "call or put ?";
              match read_line () with
                | exception End_of_file -> ()
                | call_or_put -> let date_lst =  String.split_on_char '/' date in 
              let bd = blackscholes_date date_lst in 
              let date_lst_current =  String.split_on_char '/' current_date in 
              let bd_current = blackscholes_date date_lst_current in   
              let european_option = Blackscholes.create_european_option (float_of_string strike_price) bd (float_of_string risk_free_rate) (float_of_string implied_volatility) in 
              print_endline ("-----input data-----");
              print_endline ("stock price: " ^ "$" ^ stock_price);
              print_endline ("strike price: " ^  "$" ^ strike_price);
              print_endline ("risk-free interest rate as a decimal: " ^ risk_free_rate);
              print_endline ("implied volatility as a decimal: " ^ implied_volatility);
              print_endline ( "----------------");
              if (call_or_put = "call") then (
                print_endline ("$" ^ string_of_float (Blackscholes.european_call_options_price european_option (float_of_string stock_price) bd_current));
                )
              else (
                print_endline ("$" ^ string_of_float (Blackscholes.european_put_options_price european_option (float_of_string stock_price) bd_current)) ;
                )
