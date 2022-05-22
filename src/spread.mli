(** Module containing the characteristic features of a type of option spread.
    This module represents an honest to goodness representation of a spread.*)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * This is the interface file
 **********************************************************************)
(* strike1 < strike2 < ... < strikeN *)
 type spread = 
 (*ATM w/in [2, 3]*)
 | Condor of {strike1:float; strike2:float; strike3:float; strike4:float; bwpc_list: string list}
 (*ATM at 2*)
 | Butterfly of {strike1:float; strike2:float; strike3:float; bwpc_list: string list}
 (*ATM at 2*)
 | BearPut of {strike1:float; strike2:float; bwpc_list: string list}
 | LongCall of {strike1:float; bwpc_list: string list}
 | LongPut of {strike1:float; bwpc_list: string list}
 (*ATM at 1*)
 | BullCallLadder of {strike1:float; strike2:float; strike3:float; 
                      bwpc_list: string list}
 | BullPut of {strike1:float;strike2:float;bwpc_list: string list}
 | Straddle of {strike1:float; strike2: float; bwpc_list: string list}
 | Strangle of {strike1:float; strike2: float; bwpc_list: string list}
 | FatTailedBeast of {strike1:float; strike2: float; strike3:float; 
                      strike4: float; strike5:float; strike6: float; strike7:float; 
                      strike8:float; strike9: float; bwpc_list: string list}
 | FatTailedShrimp of {strike1:float; strike2:float; strike3:float; 
                       strike4: float; strike5:float; bwpc_list: string list }
 | Gambler of {strike1:float; strike2:float; strike3:float; 
                  strike4: float; strike5:float; bwpc_list: string list }
 | RichMansPension of {strike1:float; strike2:float; strike3:float; 
                       strike4: float; strike5:float; bwpc_list: string list }
 | BearGrasp of {strike1:float; strike2:float; strike3:float; 
                       strike4: float; strike5:float; bwpc_list: string list }                   
 | MexicanCowboyHat of {strike1:float; strike2: float; strike3:float; 
 strike4: float; strike5:float; strike6: float; strike7:float; 
 strike8:float; strike9: float; strike10:float; strike11:float; strike12:float;
 bwpc_list: string list}

 (** The abstract type of values representing an option spread. *)

type t = { spread : spread;
           expiry : Blackscholes.date;
           options : Blackscholes.european_option list }
(** The abstract type of values representing a pdf function. *)

val make_spread: string -> t
(** [make_spread spread_name]   
  Prompts the user to enter information for a spread of name [spread_name],
  and returns a Spread.t with specified info. *)

val price_spread: t -> float -> Blackscholes.date -> float
(** [price_spread spreadT underlying today]   
    Calculates the price of [spreadT] on date [today], when the underlying asset 
    is at price [underlying] *)