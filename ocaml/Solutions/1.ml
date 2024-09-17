let exp_ten (x : float) : float =
  let rec helper = function
    | (_, 10., _, _, res) -> res
    | (x, t, f, p, acc) ->  helper (x, t +. 1., f *. t, p *. x, acc +. (x *. p) /. (f *. t)) 
  in 
  helper (x, 1., 1., 1., 1.);;

let xs = [1.; 2.; 3.; 4.; 5.] in 
        List.map exp_ten xs