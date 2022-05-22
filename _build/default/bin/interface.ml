open ANSITerminal
open OFinance
(* open Blackscholes
open Maths
open Levy
open Csvreader
open Montecarlo
open Csvreader
open Binomial
open Spread *)

let rec interface_main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
  "\nHello welcome to OFinance Options Pricing Model! \n\n";
  print_endline "try 'quit' or\nmake spread\nmake reader\nmake binomial\nmake option\nmake visualize \nmake arbitrage \n-------------";
    match read_line () with
    | "quit" ->  ANSITerminal.print_string [ ANSITerminal.red ] "\nThank You ! \n\n";
    | "make spread" -> Spread_loader.spread_main (); interface_main ()
    | "make reader" -> Csv_loader.main (); interface_main ()
    | "make binomial" -> Binomial_loader.main (); interface_main ()
    | "make option" -> Main.main(); interface_main ()
    | "make visualize" -> Visualize.main(); interface_main ()
    | _ -> interface_main ()

  let () =  interface_main ()

  (*
    reader:
	OCAMLRUNPARAM=b dune exec bin/csv_loader.exe
    pricing:
	OCAMLRUNPARAM=b dune exec bin/main.exe
    binomial:
	OCAMLRUNPARAM=b dune exec bin/binomial_loader.exe
    visualize:
  OCAMLRUNPARAM=b dune exec bin/visualize.exe
    *)