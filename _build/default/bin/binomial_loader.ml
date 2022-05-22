open ANSITerminal
open OFinance
open Blackscholes
open Spread
open Binomial

let blackscholes_date  lst = 
  let time = Blackscholes.create_time 0 0 0 0  in 
  Blackscholes.create_date (int_of_string (List.nth lst 0)) ( int_of_string (List.nth lst 1)) (int_of_string (List.nth lst 2)) time


let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nWelcome to Binomial options pricing model!\n";
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
        | implied_volatility -> print_endline ("Enter depth");
        match read_line () with
        | exception End_of_file -> ()
        | depth -> print_endline ("Up (as a float)");
        match read_line () with
        | exception End_of_file -> ()
        | up -> print_endline ("Down (as a float)");
        match read_line () with
        | exception End_of_file -> ()
        | down -> print_endline "Please enter the current date as (m/d/2022).";
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
              let american_option = Binomial.cons_american (float_of_string strike_price) (float_of_string risk_free_rate) bd (int_of_string depth) (float_of_string up) (float_of_string down) (float_of_string stock_price) in 
              print_endline ("-----input data-----");
              print_endline ("stock price: " ^ "$" ^ stock_price);
              print_endline ("strike price: " ^  "$" ^ strike_price);
              print_endline ("risk-free interest rate as a decimal: " ^ risk_free_rate);
              print_endline ("implied volatility as a decimal: " ^ implied_volatility);
              print_endline ("up as a decimal: " ^ up);
              print_endline ("down as a decimal: " ^ down);
              print_endline ("depth: " ^ depth);
              print_endline ( "----------------");
              if (call_or_put = "call") then print_day (american_option_price bd_current american_option) 0 0
              else ( print_endline ("$"))
         