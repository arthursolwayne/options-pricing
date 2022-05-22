open ANSITerminal
open OFinance

let blackscholes_date  lst = 
  let time = Blackscholes.create_time 0 0 0 0  in 
  Blackscholes.create_date (int_of_string (List.nth lst 0)) ( int_of_string (List.nth lst 1)) (int_of_string (List.nth lst 2)) time

let data_dir_prefix = "data" ^ Filename.dir_sep

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to the Cornell Quant Funds Blackscholes options pricing model (csv).\n";
  print_endline
    "Please enter the name of the csvfile you want to load.\n";
  print_endline "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> print_endline
  "Please enter the expiration date as (mm/dd/2022).";
  match read_line () with
    | exception End_of_file -> ()
    | date -> (*let date_lst =  String.split_on_char '/' date in 
    let bd = blackscholes_date date_lst in *)
    print_endline "csvloader print not yet working" 
    (*(Csvreader.(data_dir_prefix ^ file_name ^ ".csv") |> load_csv |> from_csv |> (Csvreader.delta bd))*)
    
