(** Module to process information for a portfolio of stocks *)

open Blackscholes

type stock_date
(** The abstract type of values representing a stock date. *)

type stock_time
(** The abstract type of values representing a stock time. *)

type ticker
(** The abstract type of values representing a ticker for a stock. *)

val portfolio_value : float list -> float list -> float -> float
(** [portfolio_value list list] produces the total value of your portfolio x*)

val average_price : float list -> float
(** [average_price list] returns the average price of a prices of a certain stock*)

val normalize_weights : float list -> float list
(** [average_price list] returns a normalized list of the weights*)

val random_stock : string list -> string 
(** [random_stock list] returns a random ticker for a stock list*)

val random_price : float list -> float 
(** [random_price list] returns a random price of a stock from its stock history*)

val estimate_risk_first_moment : float list -> float 
(** [estimate_risk_first_moment list] returns the first moment of risk estimate*)

val estimate_risk_second_moment : float list -> float 
(** [estimate_risk_first_moment list] returns the first moment of risk estimate*)

val make_stock_date : int -> int -> int -> stock_date
(** [make_stock_date int int int] returns a date object*)

val make_stock_time : int -> int -> int -> stock_time
(** [make_stock_time int int int] returns a time object*)

val make_stock_ticker : string -> ticker 
(** [make_stock_ticker string] returns a ticker object*)

val get_price_at_date : (stock_date * float) list -> stock_date -> float 
(** [get_price_at_date ((stock_date * float) list) stock_date] returns the price of a stock at a specific date*)

val get_price_at_time : (stock_time * float) list -> stock_time -> float
(** [get_price_at_time ((stock_date * float) list) stock_time] returns the price of a stock at a specific time*)

val expected_returns : float list -> float list -> float 
(** [expected_returns (float list) (float list) returns the expected returns of a portoflio with weights*)

val get_price_ticker : (ticker * float) list -> ticker -> float 
(** [get_price_ticker ((ticker * float) list) ticker returns price of stock for a certain ticker*)

val greedy_fit : (ticker * float) list -> float -> ticker list -> ticker list
(** [greedy_fit (ticker * float) list -> float takes in a list of tickers and the stock price as well as starting amount and continues to buy stockes from the list until you run out of capital.*)

val european_put_options_value : european_option list -> float -> date -> float
(** [european_put_options_value european_option list float date] takes in a list of european put options as well as stock price and date and comptues the total value of the given portfolio*)

val european_call_options_value : european_option list -> float -> date -> float
(** [european_vall_options_value european_option list float date] takes in a list of european put options as well as stock price and date and comptues the total value of the given portfolio*)

val fibonacci_analysis : float -> float -> int
(** [fibonacci_analysis float float] take two prices and computes a fibonacci analsysis*)

val am_gm : float -> float -> (float * float)
(** [am_gm float float] take two prices and computes a am_gm analsysis*)