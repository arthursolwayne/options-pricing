open Blackscholes
open Portfolio


let profit_between_dates_stocks date1 date2 (stock_data: (stock_date*float) list) : float= 
  let price_date1 = get_price_at_date stock_data date1 in 
  let price_date2 = get_price_at_date stock_data date2 in
  price_date2 -. price_date1


let profit_betwen_calloption_dates date1 date2 (european_call_option : european_option) (stock_price : float)= 
  let option_price_date1 = european_call_options_price european_call_option stock_price date1 in
  let option_price_date2 = european_call_options_price european_call_option stock_price date2 in 
  option_price_date2 -. option_price_date1

let profit_betwen_calloption_stock_price stock_price1 stock_price2 (european_call_option : european_option) (in_date : date)= 
  let option_price_price1 = european_call_options_price european_call_option stock_price1 in_date in
  let option_price_price2 = european_call_options_price european_call_option stock_price2 in_date in 
  option_price_price2 -. option_price_price1

let profit_betwen_putoption_dates date1 date2 (european_put_option : european_option) (stock_price : float)= 
  let option_price_date1 = european_put_options_price european_put_option stock_price date1 in
  let option_price_date2 = european_put_options_price european_put_option stock_price date2 in
  option_price_date2 -. option_price_date1

let profit_betwen_putoption_stock_price stock_price1 stock_price2 (european_put_option : european_option) (in_date : date)= 
  let option_price_price1 = european_put_options_price european_put_option stock_price1 in_date in
  let option_price_price2 = european_put_options_price european_put_option stock_price2 in_date in 
  option_price_price2 -. option_price_price1






