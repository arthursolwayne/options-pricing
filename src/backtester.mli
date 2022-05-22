(** Module to backtest information on both stocks and options *)

open Blackscholes
open Portfolio

val profit_between_dates_stocks : stock_date -> stock_date -> (stock_date*float)list ->float
(** [profit stock_date stock_date ((stock_date*float)list )] returns the profit made between two dates*)

val profit_betwen_calloption_dates : date -> date -> european_option -> float -> float 
(** [profit_betwen_calloption_dates date date european_option float float] returns the differnece of two call options depending on the different current date*)

val profit_betwen_calloption_stock_price : float -> float -> european_option -> date -> float
 (** [profit_betwen_calloption_stock_price  price1 price2  european_option date] returns the differnece of two call options depending on the different stock price*)

 val profit_betwen_putoption_dates : date -> date -> european_option -> float -> float 
 (** [profit_betwen_putoption_dates date date european_option float float] returns the differnece of two put options depending on the different current date*)
 
 val profit_betwen_putoption_stock_price : float -> float -> european_option -> date -> float
  (** [profit_betwen_putoption_stock_price  price1 price2  european_option date] returns the differnece of two put options depending on the different stock price*)
 
 