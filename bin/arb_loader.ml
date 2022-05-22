open OFinance

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Cornell Quant Funds abritrage loader.\n";
  print_endline
    "Please enter the type of abritrage as a string";
  match read_line () with
  | exception End_of_file -> ()
  | abritrage -> print_endline
  "Please enter the underlying price at the risk free rate.";
  match read_line () with
    | exception End_of_file -> ()
    | underlying -> print_endline
    "Please add your price of the option.";
    match read_line () with
    | exception End_of_file -> ()
    | option -> 
    print_endline ("-----input data-----");
    print_endline (string_of_bool (Arb.check_call (float_of_string underlying) (float_of_string option) ))



