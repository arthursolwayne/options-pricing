open Maths
type generator = {pdf : Maths.pdf ; init : float ; numsteps : int}

let rec walk_loop  (g : generator) (init : float) (numsteps : int) (running : float list) =
  if numsteps = 0 then running
  else let newval = (Maths.generate g.pdf +. init) in
    walk_loop g newval (numsteps-1) (newval :: running)

let walk (g : generator) =
  if g.numsteps < 0 then failwith "negative steps" else
  walk_loop g g.init g.numsteps [g.init]
