(** Module to process csv files that contain financial data in a standard format *)

type d
(** The abstract type of values representing data on a single option. *)

val parse_date : string -> Blackscholes.date
(** [parse_date d] is the date that d represents. Requires: [d] is in the format MM/DD/YYYY *)

val from_csv : Csv.t -> d list
(** [from_csv c] is the data that [c] represents. Requires: [c] is a valid options data representation *)

val load_csv : string -> Csv.t
(** [load_csv s] is the [Csv] module representation of a given csv file. *)

val get_greek : string -> d list -> string -> string -> string -> (float * float) list
(** [get_greeks greek data symbol exp side] is a list of ([strike], [value]) a given [greek] for a [date] *) 

val get_exps: d list -> string -> string list
(** [get_exps data symbol] is a set-like list of all expirations for a [symbol] *)

val first : d list -> string
(** [first data] returns first row of csv file *)
