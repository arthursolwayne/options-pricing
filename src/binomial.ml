open Blackscholes

type 'a tree = | Leaf | Node of 'a * 'a tree * 'a tree

type t = { 
  strike_price : float ; 
  risk_free_rate : float;
  expiration_date : date; 
  num_periods : int;
  up : float;
  down : float;
  current_price : float
}

let cons_american strike risk exp num up down cur = {
    strike_price = strike;
    risk_free_rate = risk;
    expiration_date = exp;
    num_periods = num;
    up = up;
    down = down;
    current_price = cur
}

let date_to_string date = (string_of_int (get_month date)) ^ "/" ^
(string_of_int (get_day date)) ^ "/" ^ (string_of_int (get_year date))

let q_calculation risk_free_rate up down time_step = (Float.exp (risk_free_rate
*. time_step) -. down) /. (up -. down)

let print_pair pair = 
    match pair with | (price, probability) -> (print_string "( "; print_string
    "Price: "; print_string (string_of_float price); print_string " Probability:
    "; print_string (string_of_float probability); print_string " )";)

let init_tree price = Node((price, 1.), Leaf, Leaf)

let rec print_tree = function 
    | Leaf -> () | Node (y, left, right) -> (print_pair y; print_tree left;
    print_tree right;)

let rec check_sum prev tree = match tree with 
  | Leaf -> prev /. 2.
  | Node ((x, y), left, right) -> check_sum y left +. check_sum y right

let rec expected_val prev tree = match tree with 
    | Leaf -> begin match prev with 
        (x,y) -> ((x *. y) /. 2.) end 
    | Node ((x, y), left, right) -> expected_val prev left +. expected_val prev right 

let is_between date0 date1 date2 = 
    if get_year date0 < get_year date1 || get_year date0 > get_year date2 then false else 
        if get_month date0 < get_month date1 || get_month date0 > get_month date2 then false else
            if get_day date0 < get_day date1 || get_day date0 > get_day date2 then false else
            true 

let create prev_data q tree up down is_up depth num_periods= 
    let prb = q in 
        let rec creator prev_data q tree up down is_up depth num_periods=
            match prev_data with 
            | (price, probability) -> if num_periods = depth then tree else 
                match tree with | Leaf -> begin 
                    match (((if is_up then price *. up else price *. down),
                    (probability *. q)), Leaf, Leaf) with | ((x, y), left,
                    right) -> Node ((x,y), creator (x, y) (1.-.prb) left up down
                    false (depth + 1) num_periods, creator (x,y) prb  right up
                    down true (depth + 1) num_periods) end 
                | Node ((x, y), left, right) -> Node ((x,y), creator (x, y)
                (1.-.prb) left up down false (depth + 1) num_periods, creator
                (x,y) prb right up down true (depth + 1) num_periods)
        in creator prev_data prb tree up down is_up depth num_periods


let max (x : float) (y : float) = if x >= y then x else y 

let rec evaluate risk_free_rate q time_step strike tree = 
    match tree with 
    | Node ((price, (probability : float)), left, right) -> 
        begin match left, right with
        | Leaf, Leaf -> (max 0. (price -. strike)) 
        | Node ((xl, yl), _, _ ), Node ((xr,yr), _, _ ) -> ((q *. evaluate risk_free_rate q time_step strike left) +. ((1. -. q) *.evaluate risk_free_rate q time_step strike right) ) *. (Float.exp ( -1. *.
    risk_free_rate *. time_step))
    (** Should never execute this statement *)
        | Node ((xl, yl), _, _ ), Leaf -> 0.
        | Leaf, Node ((xr,yr), _, _ ) -> 0.
        end 
    (** Should never execute this statement  *)
    | Leaf -> 0. 

let rec valuation_tree risk_free_rate q time_step strike = function 
    | Leaf -> Leaf
    | Node ((x, y), left, right) -> 
        Node ((evaluate risk_free_rate q time_step strike (Node((x, y), left, right)), y), 
        valuation_tree risk_free_rate q time_step strike left,
        valuation_tree risk_free_rate q time_step strike right)


let first_node tree = 
  match tree with
  | Node ((price, probability), l, r) -> print_endline (string_of_float price)
  | Leaf -> ()


let rec print_day tree depth cur_depth = 
    match tree with | Node ((price, probability), l, r) -> if cur_depth = depth
    then let () = print_endline((string_of_float price) ^ " ") in let () = (print_day l depth (cur_depth + 1)) in (print_day r depth
    (cur_depth + 1)) else let () = (print_day l depth (cur_depth + 1)) in (print_day r depth
    (cur_depth + 1)) | Leaf -> ()

exception Bad of string 

(** Just call evaluate on different depths to today's value  *)
let american_option_price cur_date option = 
    let time_step = float_of_int (diff_between_dates cur_date option.expiration_date) /. (float_of_int (option.num_periods) *. 365.) in
        let q = (q_calculation option.risk_free_rate option.up option.down time_step) in 
            let price_tree = create (option.current_price , 1.) q (init_tree option.current_price) option.up option.down false 0 option.num_periods in 
                let valuation_tree = (valuation_tree option.risk_free_rate q time_step option.strike_price price_tree) in valuation_tree

