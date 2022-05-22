(** Module for checking arbitrage pricing boundaries between different market prices for an option. 
    Will possibly read an order book and scan for arbitrage, currently checks between different prices. *)

open Blackscholes

val check_forward : float -> float -> float -> float -> bool
(** [check_forward underlying rate time forward] compares the forward stock price with the current stock price. *) 

val check_parity : float -> float -> float -> float -> float -> float -> bool
(** [check_parity underlying rate time forward] compares the forward stock price with the current stock price. *) 

val check_call : float -> float -> bool 
(** [check_call underlying call] compares the call price to the unlderying price. *)

val check_put : float -> float -> float -> float -> bool 
(** [check_put put strike rate time] compares the value of a put to the strike. *)