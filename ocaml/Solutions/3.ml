let list_replication l n = 
    let rec helper acc el =
    match acc with
    |   (list, 0, x) -> (list, x, x)  
    |   (list, x, y) -> helper (el::list, x-1, y) el in 
    let (nl,_,_) = List.fold_left helper ([], n, n) l in 
    List.rev nl;;

list_replication [1;2;3;3;4] 3