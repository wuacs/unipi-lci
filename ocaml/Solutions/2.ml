let super_digit x = 
    let rec helper = function 
      (n, acc) when n = 0 -> helper2 acc
    | (n, acc) -> helper (n/10, (n mod 10)+acc)
    and helper2 = function
      n when n < 10 -> n
    | n -> helper (n, 0)
    in helper2 x;;

super_digit(333)