let count_change d n = 
    let array = Array.make (n + 1) 0
    in 
    let lenght = Array.length d   
    in
    let rec count = function
      | (x, t) when x > n -> if t==(lenght-1) then (Array.get array n) else count (Array.get d (t+1), t+1)  
      | (x, t) -> if x - Array.get d t == 0 then begin
                      Array.set array x ((Array.get array x) + 1);
                      count (x + 1, t)
                  end else begin
                  Array.set array x ((Array.get array (x-Array.get d t)) + Array.get array x);
                  count (x+1, t);
                  end
    in
    count (Array.get d 0, 0) 
