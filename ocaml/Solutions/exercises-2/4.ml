let count_sums x n = 
  let array = Array.make_matrix (x + 1) (x + 1) (-1)
  in
  let rec count = function
    | (0, i) -> 1
    | (np, i) when np < 0 -> 0
    | (p, k) when k > truncate (ceil (Float.pow (float_of_int p) (Float.div 1.0 (float_of_int n)))) -> 0
    | (p, i) -> match array.(p).(i) with
                | t when t < 0 -> array.(p).(i) <- count (p, i+1) + count(p - truncate (Float.pow (float_of_int i) (float_of_int n)), i+1);
                                  array.(p).(i)
                | q -> q
  in
  count (x, 1)

