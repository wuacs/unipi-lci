open OUnit2
open MiniFun

(*
Testing program
(fun x : x+4) 5
*)

(* A simple test case *)
let test_eval0 _ =
  let result =
    eval
      (App
         ( Value
             (Closure ("x", Op (Var "x", Plus, Value (Integer 4)), EnvMap.empty)),
           Value (Integer 5) ))
      EnvMap.empty
  in
  match result with
  | Integer 9 -> assert_equal true true (* Test passes *)
  | _ -> assert_failure "Expected Value(Integer(9))"

(*
Testing program 
(fun x : x + 4) true

(it needs to fail!)
*)

let test_eval1 _ =
  try
    let _ =
      eval
        (App
           ( Value
               (Closure
                  ("x", Op (Var "x", Plus, Value (Integer 4)), EnvMap.empty)),
             Value (Boolean true) ))
        EnvMap.empty
    in
    assert_failure "Exception taken care"
  with
  | Failure _msg -> assert_equal true true
  | _ -> assert_failure "Exception not properly handled"

(*
Testing factorial function
(letfun factorial x: (if x == 0 then 1 else (factorial (x Minus 1)) Times x)) in factorial 5
*)

let test_eval2 _ =
  let internal =
    If
      ( Op (Var "x", Equal, Value (Integer 0)),
        Value (Integer 1),
        Op
          ( Var "x",
            Mul,
            App (Var "factorial", Op (Var "x", Minus, Value (Integer 1))) ) )
  in
  let program =
    LetFun ("factorial", "x", internal, App (Var "factorial", Value (Integer 5)))
  in
  match eval program EnvMap.empty with
  | Integer 120 -> assert_equal true true
  | _ -> assert_failure "Expected factorial of 5 to be 120"

(* Test suite *)
let suite =
  "MiniFun Test Suite"
  >::: [
         "summing 4 and 5... " >:: test_eval0;
         "summing 4 and true... " >:: test_eval1;
         "calculating factorial of 5... " >:: test_eval2;
       ]

(* Run the test suite *)
let () = run_test_tt_main suite
