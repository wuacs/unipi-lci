let is_function l = 
    let hash = Hashtbl.create 10000 
    in
    let rec helper = function
    | [] -> true
    | (x, y)::list -> try 
                    let y' = Hashtbl.find hash x in
                    y = y' (* check for same image *)
                with my_exception -> 
                    let _ = Hashtbl.add hash x y in
                    helper list
    in
    helper l;;

is_function [(1,2); (2,4); (3,6); (4,8); (1,0)]