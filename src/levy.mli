
open Maths
open Blackscholes
(** Module containing mathematical processes necessary for 
    stochastic levy processes
    This module represents ...*)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * This is the interface file
 **********************************************************************)

 (** The abstract type of values representing a class of pdf functions. *)

type generator = {pdf : Maths.pdf ; init : float ; numsteps : int}
(** The abstract type of values representing a pdf function. *)

val walk : generator -> float list
