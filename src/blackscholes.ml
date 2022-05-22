open Maths

type time = {
  hour: int; 
  minute : int ;
  seconds : int ;
  milliseconds : int
}

type date = { 
  month : int ;
  day : int ;
  year : int ;
  time : time 
}

type european_option = { 
  strike_price : float ; 
  exercise_date : date ; 
  risk_free_rate : float;  
  implied_volatility : float;
}

let create_time h m s ms = {
  hour = h; minute = m; seconds = s; milliseconds = ms
}

let get_day d = d.day

let get_month d = d.month

let get_year d = d.year 

let create_date m d y (t: time)  = {month = m; day = d; year = y; time = t}

let date_to_string date = (string_of_int date.month ^ "/" ^ string_of_int date.day ^ "/" ^ string_of_int date.year)

let create_european_option (k:float) (d:date) (r:float) (v:float) = { 
  strike_price = k ; 
  exercise_date = d; 
  risk_free_rate = r; 
  implied_volatility = v
}

(*[days_of_month m] returns the number of days in a given month. Requires m in between [0,12] *)
let days_of_month m = 
  if m = 1 ||  m = 3 || m = 5 ||  m = 7 ||  m = 8 ||  m = 10 ||  m = 12 then 31 else if m = 2 then 28 else 30

(* [diff_between_dates date1 date2] is the number of days (date1 .... date2] between date1 and date2.
  Requires: date1 and date2 are either the same year or in directly adjacent years. date2 comes after date1.  *)
let rec diff_between_dates date1 date2 =
  if date1.month = date2.month && date1.year = date2.year then 1 + date2.day -
  date1.day else if date1.year = date2.year && date2.month <> date1.month then 1
  + days_of_month date1.month - date1.day + diff_between_dates {year =
  date1.year; month = date1.month + 1; day = 1; time = date1.time } date2 else
  diff_between_dates date1 {month = 12; day = 31; year=date1.year;
  time=date1.time} + diff_between_dates {month = 1; day = 1; year=date2.year;
  time=date2.time} date2

(* [d1] computes the d1 part of the black-scholes equation *)
let d1 (european_option : european_option) (current_stock_price : float) (time_to_expiry : float )  : float =  
  let num = Float.log (current_stock_price /. european_option.strike_price ) +. ((european_option.risk_free_rate  +. 
  ((european_option.implied_volatility **2.) /. 2.)) *. time_to_expiry)in  
  let den = european_option.implied_volatility  *. Float.sqrt time_to_expiry in (num/.den)

let d2 (european_option : european_option) (d1 : float) (time_to_expiry : float) : float = 
  d1 -. european_option.implied_volatility *.Float.sqrt time_to_expiry

let european_call_options_price (european_call : european_option) (current_stock_price : float) (current_date : date) = 
  let time_to_expiry = (float_of_int (diff_between_dates current_date european_call.exercise_date )) /. 365.0 in 
  let d1 = d1 european_call current_stock_price time_to_expiry in 
  let d2 = (d2 european_call d1 time_to_expiry ) in 
  let a_normal_pdf =
  {functn = (fun x -> Float.sqrt(1. /. (2.*.Float.pi)) *. Float.exp(-1. *. x *. x /. ((1.0*.1.0)))) ; 
  (* stddev = 1 ; mean = 0 *)
  distribution_class = Maths.Normal {stddev = 1.0; mean = 0.}} in
  let term1 = current_stock_price *. (Maths.cdf a_normal_pdf d1) in 
  let term2 = european_call.strike_price *. 
  Float.exp ( -1. *. european_call.risk_free_rate *. time_to_expiry) *. Maths.cdf a_normal_pdf d2 in 
  term1 -. term2

let european_put_options_price (european_put : european_option) (current_stock_price : float) (current_date : date) = 
  let time_to_expiry = (float_of_int (diff_between_dates current_date european_put.exercise_date )) /. 365.0 in 
  let d1 = d1 european_put current_stock_price time_to_expiry in 
  let d2 = (d2 european_put d1 time_to_expiry ) in 
  let a_normal_pdf =
  {functn = (fun x -> Float.sqrt(1. /. (2.*.Float.pi)) *. Float.exp(-1. *. x *. x /. ((1.0*.1.0)))) ; 
  (* stddev = 1 ; mean = 0 *)
  distribution_class = Maths.Normal {stddev = 1.0; mean = 0.}} in
  let term2 = current_stock_price *. (Maths.cdf a_normal_pdf (-1. *.d1)) in 
  let term1 = european_put.strike_price *. 
  Float.exp ( -1. *. european_put.risk_free_rate *. time_to_expiry) *. Maths.cdf a_normal_pdf (-1.*.d2) in 
  term1 -. term2

