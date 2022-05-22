open ANSITerminal
open Blackscholes
open Stdlib

type spread = 
| Condor of {strike1:float; strike2:float; strike3:float; strike4:float; bwpc_list: string list } 
| Butterfly of {strike1:float; strike2:float; strike3:float; bwpc_list: string list}
| BearPut of {strike1:float; strike2:float; bwpc_list: string list}
| LongCall of {strike1:float; bwpc_list: string list}
| LongPut of {strike1:float; bwpc_list: string list}
| BullCallLadder of {strike1:float; strike2:float; strike3:float; bwpc_list : string list}
| BullPut of {strike1:float;strike2:float;bwpc_list: string list}
| Straddle of {strike1:float; strike2: float; bwpc_list: string list}
| Strangle of {strike1:float; strike2: float; bwpc_list: string list}
| FatTailedBeast of {strike1:float; strike2: float; strike3:float; strike4: 
float;strike5:float; strike6: float; strike7:float; strike8:float; strike9: float; bwpc_list: string list}
| FatTailedShrimp of {strike1:float; strike2:float; strike3:float; strike4: float; strike5:float; bwpc_list: string list }
| Gambler of {strike1:float; strike2:float; strike3:float; strike4: float; strike5:float; bwpc_list: string list }
| RichMansPension of {strike1:float; strike2:float; strike3:float; strike4: float; strike5:float; bwpc_list: string list }
| BearGrasp of {strike1:float; strike2:float; strike3:float; strike4: float; strike5:float; bwpc_list: string list }
| MexicanCowboyHat of {strike1:float; strike2: float; strike3:float; strike4: 
float;strike5:float; strike6: float; strike7:float; strike8:float; strike9: float; 
strike10:float; strike11:float; strike12: float; bwpc_list: string list}

type t = { spread : spread; expiry : Blackscholes.date; options : european_option list}

let get_spread_bwpc_list = function
| Condor {strike1; strike2; strike3; strike4; bwpc_list} -> ["bc1"; "wc1"; "wc1"; "bc1"]
| Butterfly {strike1; strike2; strike3; bwpc_list} -> ["bc1"; "wc2"; "bc1"]
| BearPut {strike1; strike2; bwpc_list} -> ["wp1"; "bp1"]
| LongCall {strike1; bwpc_list} -> ["bc1"]
| LongPut {strike1; bwpc_list} -> ["bp1"]
| BullCallLadder{strike1; strike2; strike3; bwpc_list} -> ["bc1"; "wc1"; "wc1"]
| BullPut{strike1;strike2;bwpc_list;} -> ["bp1";"wp1"]
| Straddle {strike1;strike2;bwpc_list;} -> ["bp1";"bc1"]
| Strangle {strike1;strike2;bwpc_list;} -> ["wp1";"wc1"]
| FatTailedBeast {strike1; strike2; strike3; strike4;
strike5; strike6; strike7; strike8; strike9; bwpc_list} ->
  ["bp1"; "bp1"; "bp1"; "bc2";"wc4";"bc2";"bc1"; "bc1"; "bc1"]
| FatTailedShrimp {strike1; strike2; strike3; strike4; strike5; bwpc_list} -> ["bp1"; "bc1"; "wc2"; "bp1"; "bc1"] 
| Gambler {strike1; strike2; strike3; strike4; strike5; bwpc_list} -> ["wp1"; "wp1"; "wc2"; "wc1"; "wc1"] 
| RichMansPension {strike1; strike2; strike3; strike4; strike5; bwpc_list} -> ["wp2"; "wp1"; "bc2"; "bp1"; "bc1"] 
| BearGrasp {strike1; strike2; strike3; strike4; strike5; bwpc_list} -> ["bp2"; "bp1"; "bp1"; "wp1"; "wp1"] 
| MexicanCowboyHat {strike1; strike2; strike3; strike4; strike5; strike6; 
strike7; strike8; strike9; strike10; strike11; strike12; bwpc_list} -> 
  ["bp1";"bp1";"bp1"; "bc1";"wc1";"wc1"; "bc1";"wc1";"bc1"; "bc1";"bc1";"bc1"]

let blackscholes_date  lst = 
  let time = Blackscholes.create_time 0 0 0 0  in 
  Blackscholes.create_date 
  (int_of_string (List.nth lst 0)) 
  (int_of_string (List.nth lst 1)) 
  (int_of_string (List.nth lst 2)) time

let rec get_strikes (num_strikes : int) = 
  if num_strikes = 0 then [] else
  match read_line () with
  | strike ->  let s = float_of_string_opt strike in
    match s with
    | Some flt -> flt :: get_strikes (num_strikes-1) 
    | None -> get_strikes num_strikes
    
let rec make_options (strikes : float list) (expiry : date) (r:float) (v:float) =
  match strikes with 
  | [] -> []
  | h :: t -> (create_european_option h expiry r v) :: make_options t expiry r v

let make_t (name : string) (expiry : date) (r : float) (v : float)  = 
  print_endline "Please enter the strike prices of the options in increasing order.";
  match name with
    | "condor" -> ( let strikes = (get_strikes 4) in 
      let option_spread = 
        Condor {
          strike1 = List.nth strikes 0; 
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2; 
          strike4 = List.nth strikes 3; 
          bwpc_list = ["bc1"; "wc1"; "wc1"; "bc1"]} in 
      {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "butterfly" -> (
      let strikes = get_strikes 3 in 
      let option_spread =  Butterfly 
        {strike1 = List.nth strikes 0;
        strike2 = List.nth strikes 1;
        strike3 = List.nth strikes 2;
        bwpc_list = ["bc1"; "wc2"; "bc1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "bear put" -> (
      let strikes = get_strikes 2 in 
      let option_spread = BearPut 
        {strike1 = List.nth strikes 0; 
        strike2 = List.nth strikes 1;
        bwpc_list = ["wp1"; "bp1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "long call" -> (
      let strikes = get_strikes 1 in 
      let option_spread = LongCall
      {strike1 = List.nth strikes 0;
      bwpc_list = ["bc1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "long put" -> (
      let strikes = get_strikes 1 in 
      let option_spread = LongPut
      {strike1 = List.nth strikes 0;
      bwpc_list = ["bp1"]} in
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "bull call ladder" -> (
      let strikes = get_strikes 3 in 
      let option_spread =  BullCallLadder 
        {strike1 = List.nth strikes 0;
        strike2 = List.nth strikes 1; 
        strike3 = List.nth strikes 2;
        bwpc_list = ["bc1"; "wc1"; "wc1"]} in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "bull put" -> (
        let strikes = get_strikes 2 in 
        let option_spread =  BullPut
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          bwpc_list = ["bp1"; "wp1"]} in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "straddle" -> (
        let strikes = get_strikes 2 in 
        let option_spread =  Straddle
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          bwpc_list = ["bp1";"bc1"]} in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "strangle" -> (
          let strikes = get_strikes 2 in 
          let option_spread = Strangle
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          bwpc_list = ["wp1";"wc1"]} in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "fat tailed" -> (
          let strikes = get_strikes 9 in 
          let option_spread =  FatTailedBeast
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2;
          strike4 = List.nth strikes 3; 
          strike5 = List.nth strikes 4;
          strike6 = List.nth strikes 5; 
          strike7 = List.nth strikes 6;
          strike8 = List.nth strikes 7;
          strike9 = List.nth strikes 8;
          bwpc_list = ["bp1"; "bp1"; "bp1"; "bc2";"wc4";"bc2";"bc1"; "bc1"; "bc1"]} in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "ancient spread" -> (
          let strikes = get_strikes 5 in 
          let option_spread =  FatTailedShrimp
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2;
          strike4 = List.nth strikes 3; 
          strike5 = List.nth strikes 4;
          bwpc_list = ["bp1"; "bc1"; "wc2"; "bp1"; "bc1"] } in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    |  "lucky spread" -> (
          let strikes = get_strikes 5 in 
          let option_spread =  Gambler
         {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2;
          strike4 = List.nth strikes 3; 
          strike5 = List.nth strikes 4;
          bwpc_list = ["bp1"; "bc1"; "wc2"; "bp1"; "bc1"] } in 
        {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    |  "rich spread" -> (
          let strikes = get_strikes 5 in 
          let option_spread =  RichMansPension
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2;
          strike4 = List.nth strikes 3; 
          strike5 = List.nth strikes 4;
          bwpc_list = ["wp2"; "wp1"; "bc2"; "bp1"; "bc1"]  } in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    |  "bear grasp" -> (
          let strikes = get_strikes 5 in 
          let option_spread =  BearGrasp
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2;
          strike4 = List.nth strikes 3; 
          strike5 = List.nth strikes 4;
          bwpc_list = ["bp2"; "bp1"; "bp1"; "wp1"; "wp1"]  } in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | "cowboy spread" -> (
          let strikes = get_strikes 12 in 
          let option_spread =  MexicanCowboyHat
          {strike1 = List.nth strikes 0;
          strike2 = List.nth strikes 1; 
          strike3 = List.nth strikes 2;
          strike4 = List.nth strikes 3;             
          strike5 = List.nth strikes 4;
          strike6 = List.nth strikes 5; 
          strike7 = List.nth strikes 6;
          strike8 = List.nth strikes 7;
          strike9 = List.nth strikes 8;
          strike10 = List.nth strikes 9;
          strike11 = List.nth strikes 10;
          strike12 = List.nth strikes 11;
          bwpc_list = ["bp1";"bp1";"bp1"; "bc1";"wc1";"wc1"; "bc1";"wc1";"bc1"; "bc1";"bc1";"bc1"]} in 
          {spread = option_spread; expiry = expiry; options = make_options strikes expiry r v})
    | _ -> failwith ("Spread not recognized")

let rec price_options (options : european_option list) 
(bwpc_list : string list) (underlying : float) (today : date) = 
match options with
| [] -> 0.
| h :: t ->
  match bwpc_list with
    | [] -> failwith "invalid bwpc list"
    | info :: tl -> let code = String.sub info 0 2 and num = String.sub info 2 1 in
    match code with
      | "bc" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> 
        (flt *. (european_call_options_price h underlying today) +.
         price_options t tl underlying today))
      | "bp" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> 
        (flt *. (european_put_options_price h underlying today) +. 
        price_options t tl underlying today) )
      | "wc" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> 
        (-1. *. flt *. (european_call_options_price h underlying today) +. 
        price_options t tl underlying today) )
      | "wp" -> (match float_of_string_opt num with 
        | None -> failwith "invalid bwpc string"
        | Some flt -> 
        (-1.*. flt *. (european_put_options_price h underlying today) +. 
        price_options t tl underlying today) )
      | _ -> failwith "invalid bwpc string"

      
let price_spread (spread : t) (underlying : float) (today : date) = 
  match spread with
  | {spread; expiry; options} -> price_options options (get_spread_bwpc_list spread) underlying today


let make_spread (spread : string) =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nThank you for choosing to buy a spread !\n";
  let name = String.lowercase_ascii spread in    
  print_endline("Spread type: " ^ name); 
  print_endline "\nPlease enter the expiry of this spread as (mm/dd/2022).";
  match read_line () with
    | date -> let expiry = blackscholes_date (String.split_on_char '/' date) in
    print_endline
      "Please enter the risk free interest rate (Usually 30-year US Treasury bond yield [.0248]).";
    match read_line () with
      | r -> print_endline
      ("Please enter the implied volatility (annualized standard deviation of asset returns)\n" ^
      "or: 0.1 -> 0.2 : not volatile, 0.2 -> 0.4 : fairly volatile, 0.4+ : highly volatile.");
      match read_line () with
        | v -> make_t name expiry (float_of_string r) (float_of_string v)