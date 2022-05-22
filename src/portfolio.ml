open Blackscholes

type stock_date = { 
  month : int ;
  day : int ;
  year : int ;
}

type stock_time = { 
  hour : int ;
  minute : int ;
  second : int ;
}

type ticker = { 
 t : string
}

let make_stock_date m d y = 
  {month = m; day = d; year = y}

let make_stock_time h m s = 
  {hour = h; minute = m; second = s}

let make_stock_ticker tick = 
  {t = tick}
  
let rec portfolio_value (stock_prices: float list) (weights : float list) (acc : float) = 
  match (stock_prices,weights) with 
  | [],[] -> acc
  | [] , _ -> acc 
  | _ , [] -> acc 
  | (h1::t1, h2::t2) -> portfolio_value t1 t2 (acc +. h1*.h2)
  
let average_price (stock_prices : float list) = 
  (List.fold_left (fun acc v -> acc +. v) 0. stock_prices) /. (float_of_int (List.length stock_prices))

let normalize_weights (portfolio_weights : float list) = 
  let square_of_sums =  Float.sqrt (List.fold_left (fun acc v -> acc ** 2. +. v) 0. portfolio_weights) in 
  List.map (fun v -> v /. square_of_sums) portfolio_weights

let random_stock (stocks : string list) = 
  let rand = Random.full_int (List.length stocks) in 
  List.nth stocks rand

let random_price (stock_prices : float list) = 
  let rand = Random.full_int (List.length stock_prices) in 
  List.nth stock_prices rand

let estimate_risk_first_moment (stock_prices : float list)  = 
  let avg_price = average_price stock_prices in 
  let rand_price = random_price stock_prices in 
  Float.abs (rand_price -. avg_price)

let estimate_risk_second_moment(stock_prices : float list)  = 
  let avg_price = average_price stock_prices in 
  let rand_price = random_price stock_prices in 
  Float.abs (rand_price **2. -. avg_price ** 2.)

let rec get_price_at_date (stock_data : (stock_date * float) list ) input_date =
  match stock_data with 
  | [] -> 0.
  | ({month = m;day = d;year = y},v) :: t -> if ( (m = input_date.month) && (d = input_date.day) && (y = input_date.year)) then v else get_price_at_date t input_date

let rec get_price_at_time (stock_data : (stock_time * float) list ) input_time =
  match stock_data with 
    | [] -> 0.
    | ({hour = h; minute = m; second = s},v) :: t -> if ( (h = input_time.hour) && (m = input_time.minute) && (s = input_time.second)) then v else get_price_at_time t input_time
  
let rec get_price_ticker (stock_data : (ticker * float ) list) (input_ticker:ticker) = 
  match stock_data with 
  | [] -> 0.
  | ({t = ticker},v)::tail -> if input_ticker.t = ticker then v else get_price_ticker tail input_ticker

let rec greedy_fit (stock_price: (ticker*float) list) (amount:float) (acc : ticker list) = 
  match stock_price with 
  | [] -> acc
  | ({t = sym} , v)::t -> if (amount > v) then greedy_fit t (amount -. v) ({t = sym}::acc) else greedy_fit t amount acc 

let expected_returns (average_daily_returns : float list) (weights : float list) = 
  let expected_return_list = List.map2 (fun expected_value w -> expected_value*. w) average_daily_returns weights in 
  List.fold_left (fun acc x -> x+. acc) 0. expected_return_list 

let rec european_put_options_value (put_options_list: european_option list) (stock_price : float) (date : date) = 
  match (put_options_list) with 
  | [] -> 0. 
  | put :: t -> european_put_options_price put stock_price date +. (european_put_options_value t stock_price date)

let rec european_call_options_value (call_options_list: european_option list) (stock_price : float) (date : date) = 
  match (call_options_list) with 
    | [] -> 0. 
    | call :: t -> european_call_options_price call stock_price date +. (european_call_options_value t stock_price date)
  
let fibonacci_analysis (new_price:float) (old_price:float) = 
  let per = new_price /. old_price *. 100.0 in
  if per < 38.2 then 1 else if per < 50.0 then 2 else if per < 61.8 then 3 else 4;;

let am_gm (x:float) (y:float) = (0.5*.(x+.y), Float.sqrt(x *. y));;
    

