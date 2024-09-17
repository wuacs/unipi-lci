let list_replication1 l n = 
    let rec helper nl ol (t, i) = 
    match (t, i) with
    | (0, _) -> nl
    | (t, 0) -> helper (ol.(0)::nl) ol (t-1, Array.length ol - 1)
    | (t, i) -> helper (ol.(i)::nl) ol (t, i-1)
    in
    helper [] (Array.of_list l) (n, List.length l - 1);;

list_replication1 [1;2;3;4] 3