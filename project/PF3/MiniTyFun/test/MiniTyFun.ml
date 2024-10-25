open OUnit2
open MiniTyFun (* Assuming MiniFun is your library module *)

(*
Type checking 5+4
*)
let test_eval0 _ = 
  let ast = Op(Val(Integer(5)), Plus, Val(Integer(4))) in
  (match type_check ast with
  | Some(Integer_t) -> assert_equal true true
  | _ -> assert_failure "Sum of integers did not give an integer")


(*
Type checking 5+((Letfun factorial n : Integer = if n == 0 then 1 else n*(factorial n - 1)) 5)
*)

let test_eval1 _ = 
  let ast = LetFun("factorial", "n", Integer_t, If(Op(Var("n"), Equal, Val(Integer(0))), Val(Integer(1)), 
            Op(Var("n"), Mul, App(Var("factorial"), Op(Var("n"), Minus, Val(Integer(1)))))), Op(Val(Integer(5)), Plus, App(Var("factorial"), Val(Integer(5)))))
  in
  (match type_check ast with
  | Some(Integer_t) -> assert_equal true true
  | _ -> assert_failure "5+factorial call should give integer")


(* Test suite *)
let suite =
  "MiniFun Test Suite"
  >::: [
         "summing 5 and 4... " >:: test_eval0;
         "summing 5 and factorial(5).." >:: test_eval1
       ]

(* Run the test suite *)
let () = run_test_tt_main suite

