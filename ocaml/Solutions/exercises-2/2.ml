let print_pascal k = 
    let rec helper = function 
    | (x::[0], t, n) -> print_int x; helper2 (List.rev (0::(x::n)), t-1) 
    | (x::xs, t, n) -> print_int (x+(List.hd xs)); print_string " "; helper (xs, t, (x+(List.hd xs))::n)
    | (_, _, _) -> () (* should not get here *)
    and helper2 = function
    | (_, 0) -> print_newline();
    | (s, t) -> print_newline(); helper (s, t, [0])
    in 
    let helper3 = function 
    | k when k<=0 -> ()
    | _ -> print_int 1; helper2 ([0;1;0], k - 1)
    in 
    helper3 k