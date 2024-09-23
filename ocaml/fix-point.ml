(* Church numerals *)
type church = Ch of ((church -> church) -> church -> church)

let add1 (Ch n) = Ch (fun f -> fun x -> f (n f x))
let zero = Ch (fun f -> fun x -> x)
let one = add1 zero
let two = add1 one
let three = add1 two
let four = add1 three
let five = add1 four
let six = add1 five
let seven = add1 six
let eight = add1 seven
let nine = add1 eight
let ten = add1 nine

let add n (Ch m) = m (add1) n
let mul n (Ch m) = m (add n) zero
let mul1 (Ch n) (Ch m) = Ch(fun s -> fun z -> m (n s) z)
let exp n (Ch m) = m (mul n) one

(* conditional test with thunks for lazy eval *)
(* thunks are objects of type church, although not *)
(* Church numerals *)
let thunk x : church = Ch (fun _ -> fun _ -> x)
let eval (Ch x) = x (fun z -> z) zero
let if_zero (Ch n) x y = eval (n (fun _ -> y) x)

(* pairs *)
(* pairs are also objects of type church, but not *)
(* Church numerals *)
let pair x y = Ch (fun _ -> fun z -> if_zero z (thunk x) (thunk y))
let fst (Ch p) = p (fun x -> x) zero
let snd (Ch p) = p (fun x -> x) one

(* subtraction *)
let next p = pair (snd p) (add1 (snd p))
let sub1 (Ch n) = fst (n next (pair zero zero))
let sub n (Ch m) = m sub1 n

(* booleans *)
let truel = one
let falsel = zero
let andl x y = mul x y
let orl x y = add x y
let notl x = if_zero x (thunk truel) (thunk falsel)
let le x y = notl (sub x y)
let lt x y = notl (le y x)
let eq x y = (andl (le x y) (le y x))

(* lazy conditional *)
let ite b x y = if_zero b y x

(* lists *)
let null = one
let is_null (Ch s) = lt (s (fun x -> x) zero) (s add1 zero) 

(* recursion *)
type 'a fix = Fix of ('a fix -> 'a)
(* typed version of the fixpoint combinator y *)
let y t = let p (Fix f) x = t (f (Fix f)) x in p (Fix p)

let fact =
  let t_fact f x =
    (if_zero x (thunk one)
               (Ch (fun _ -> fun _ -> (mul x (f (sub1 x))))))
  in y t_fact

let fib =
  let t_fib f x =
    (ite (lt x two) (thunk x)
    (Ch (fun _ -> fun _ -> (add (f (sub1 x)) (f (sub x two))))))
  in y t_fib

let length =
  let t_length f x =
    ite (is_null x) (thunk zero)
    (Ch (fun _ -> fun _ -> (add1 (f (snd x)))))
  in y t_length

(* conversions Church -> int and int -> Church
 * for convenience only -- these are not part of the encoding *)

let rec to_church n = if n = 0 then zero else add1 (to_church (n - 1))

let rec from_church (Ch n) =
  try ignore (n (fun _ -> failwith "") zero); 0
  with _ -> 1 + from_church (sub1 (Ch n))