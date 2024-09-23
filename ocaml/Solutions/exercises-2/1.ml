let swap_adjacent s = 
    let helper = function
    | x when x mod 2 = 0 -> s.[x+1]
    | x -> s.[x-1]
    in 
    String.init (String.length s) (fun i -> helper i)