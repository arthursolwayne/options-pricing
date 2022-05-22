(** Module containing mathematical processes necessary for pricing.*)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * This is the interface file
 **********************************************************************)

 type pdf_class = 
 | Normal of {stddev : float; mean : float} 
 | LogNormal of {sigma_of_log : float; mean_of_log : float}
 | Lorentzian of {gamma : float ; peak : float}
 | Laplace of {lambda : float; peak : float}
 | Other
 
(** The abstract type of values representing a class of pdf functions. *)

type pdf = { functn : float -> float ; distribution_class : pdf_class ;}
(** The abstract type of values representing a pdf function. *)

val pdf_value : pdf -> float -> float
(** [pdf_value pdf x] produces the value of the pdf function at the value x*)

val cdf : pdf -> float -> float
(** [cdf pdf x] 
  integrates the function [pdf] from -inf to upper bound [x]. 
  This is the value of the cdf of the distribution at x
  Requires: 
  pdf is C0 smooth over [a, b] and integrates to 1 *)

val integrate : pdf -> float -> float -> float
(** [integrate pdf a b] 
  integrates the function [pdf] from [a] to upper bound [b]. 
  Requires: 
  pdf is C0 smooth over [a, b]
  a <= b *)

val generate : pdf -> float
(** [generate pdf] 
   generates an instantiation of a float random variable 
   distributed along pdf *)