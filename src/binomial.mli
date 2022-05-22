(** Representation of the Binomial Option Pricing Model.*)

open Blackscholes

type t
(** t is the abstract type represnting an american option. *)

type 'a tree
(** 'a tree is the abstract type repreenting a binomial pricing tree. *)

val cons_american : float -> float -> date -> int -> float -> float -> float -> t 
(** [cons_american strike risk exp num up down cur] creates an american option. *)

val american_option_price : date -> t -> (float * float) tree 
(** [american_option_price cur_date option] creates the option price tree. *)

val print_tree : (float * float) tree -> unit 
(** [print_tree tree] prints the option price tree. *)

val valuation_tree :  float -> float -> float -> float -> (float * float) tree -> (float * float) tree
(** [valuation_tree risk_free_rate q time_step strike] creates an option valuation tree. *)

val create : (float * float) -> float -> (float * float) tree -> float -> float
-> bool -> int -> int -> (float * float) tree
(** Given an intiial tree [create prev_data q tree up down is_up depth num_periods] creates an option pricing tree. *)

val expected_val : (float * float ) -> (float * float) tree -> float 
(** [expected_val prev tree] calcualtes the expected value of an option at that node. *)

val is_between : date -> date -> date -> bool
(** [is_between date0 date1 date2] checks if date0 is etween date1 and date2.*)

val init_tree : float -> (float * float) tree
(** [init_tree price] creates a new tree with a given stock price at the start. *)

val check_sum : float -> (float * float) tree -> float 
(** [check_sum prev tree] checks that the nodes at a given depth sum to 1.*)

val first_node : (float * float) tree -> unit 
(** [first_node tree] prints the first node. *)

val print_day : (float * float) tree -> int -> int -> unit 
(** [print_day tree depth cur_depth] prints a given depth of a tree. *)

exception Bad of string