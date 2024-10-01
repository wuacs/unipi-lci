(* Variant type for functions, only those operands are allowed *)
type fn =
  | Unary of (int -> int)
  | Binary of (int -> int -> int)
  | Ternary of (int -> int -> int -> int)

  exception WrongOperands
  exception NotAnArithmeticExpression
  exception ArityNonSupported
  let arithmetic expr = 
    let pieces = String.split_on_char ' ' expr
in
    let apply f xs = 
      match f with
      | Ternary(funct) -> (match xs with 
                          | x::y::z::r -> (funct x y z)::r
                          | _ -> raise NotAnArithmeticExpression) 
      | Binary(funct) -> (match xs with 
                          | x::y::r -> (funct x y)::r
                          | _ -> raise NotAnArithmeticExpression) 
      | Unary(funct) -> match xs with 
                          | [] -> raise NotAnArithmeticExpression 
                          | x::r -> (funct x)::r
in
    let rec eval pieces s_ar s_op s_fn =
    match pieces with
    | "3+"::z -> eval z (3::s_ar) s_op (Ternary(fun l p q -> l + p + q)::s_fn)
    | "+"::z -> eval z (2::s_ar) s_op (Binary(fun l p -> l + p)::s_fn)
    | "*"::z -> eval z (2::s_ar) s_op (Binary(fun l p -> l * p)::s_fn)
    | n::z -> let num = (int_of_string n) in
              if List.hd s_ar = 1 then 
                eval z (List.tl s_ar) (apply (List.hd s_fn) (num::s_op)) (List.tl s_fn)
              else
                eval z ((List.hd s_ar - 1)::List.tl s_ar) (num::s_op) s_fn
    | [] -> if  (List.length s_ar <> 0 && (List.length s_fn = 0)) || 
                ((List.length s_op <> 1) && (List.length s_fn = 0)) then
                raise NotAnArithmeticExpression
            else 
              if List.length s_op = 1 then 
                List.hd s_op  
              else begin
                eval [] (List.tl s_ar) (apply (List.hd s_fn) s_op) (List.tl s_fn) (* Recursive call *)
              end
in
    eval pieces [] [] []

let () = 
Printf.printf "Value : %d\n" (arithmetic "+ 5 * 3 + 4 1");  (* This is equivalent to (5 + (3 * (4 + 1)) *)
Printf.printf "Value: %d\n" (arithmetic "+ 5 * 3 3+ * 4 5 1 5") (* Equivalent to 5 + (3 * ((4 * 5) + 1 + 5)) *)
