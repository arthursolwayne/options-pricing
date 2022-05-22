(** Representation of Black–Scholes Option Pricing Model. *)
    
type european_option 
(** The abstract type of values representing an european option. *)

type time 
(** The abstract type of values representing a time. *)

type date
(** The abstract type of values representing a date. *)

val get_month : date -> int
(** [get_month date] returns month as integer*) 

val get_day : date -> int
(** [get_day date] returns month as integer*) 

val get_year : date -> int 
(** [get_year date] returns month as integer*) 

val create_time : int -> int -> int -> int -> time
(** [create_time h m s ms] creates a time type. 
  Requires: 
  hours (h) : between [0,24]
  minute (m): between [0,60] 
  seconds (s) : between [0,60]
  milliseconds (s) : between [0,1000]*)

val create_date : int -> int -> int -> time -> date
(** [create_date m d y t] 
  Requires:
  month (m) : to be a valid number between [0,12]
  day (d) : requires d to be a valid date for the month 
  time (t) : required time to be a valid 24 hour time *)

val date_to_string : date -> string
(** [date_to_string date] converts [date] to string format. *)

val create_european_option : float -> date -> float -> float -> european_option
(** [create_european_option k t r v ] creates am european_option. 
    Requires:  
    strike price (k): dollars 
    exercise_date (t): in date-time format 
    risk free rate (r): percentage in decimal (i.e 2% = 0.02)
    implied volatility (v) : percentage in decimal (i.e 30% = 0.03) *)

val diff_between_dates : date -> date -> int 
(** [diff_between_dates date1 date2]
takes two valid dates and computes the time in between in days inclusive.
example: one month would be 28, 30, 31*)

val european_call_options_price : european_option -> float -> date -> float
(** [european_call_options_price call european option ] computes estimates the price of a European call option. *)

val european_put_options_price : european_option -> float -> date -> float
(** [european_put_options_price call european option ] computes estimates the price of a European put option. *)


