open Blackscholes

let close_enough a b = 
  (* print_endline (string_of_float(Float.abs (a -. b))); *)
  Float.abs (a -. b) < 1e-2

let check_forward underlying rate time forward = close_enough forward
    (underlying *. Float.exp (rate *. (time /.  365.)))

let check_parity unlderying put call strike rate time = 
    close_enough (unlderying +. put) (call +. strike *. Float.exp (-1. *. rate *. (time
    /. 365. )))

let check_call underlying call = 
    call *. 100. <= underlying 

let check_put put strike rate time = 
    put *. 100. <= strike *. Float.exp (-1. *. rate *. (time /. 365.))