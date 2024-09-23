let mingle_string p q = 
    let rec helper = function
        | i when i mod 2 = 0 -> p.[i / 2]
        | i -> q.[(i - 1) / 2] 
    in 
    String.init (String.length p * 2) (fun i -> helper i)
    