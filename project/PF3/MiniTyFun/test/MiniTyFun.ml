open OUnit2
open MiniTyFun (* Assuming MiniFun is your library module *)

(*
Type checking 5+4
*)
let test_eval0 _ =
  let ast = Op (Val (Integer 5), Plus, Val (Integer 4)) in
  match type_check ast with
  | Some Integer_t -> assert_equal true true
  | _ -> assert_failure "Sum of integers did not give an integer"

(*
Type checking 5+((Letfun factorial n : Integer = if n == 0 then 1 else n*(factorial n - 1)) 5)
*)

let test_eval1 _ =
  let ast =
    LetFun
      ( "factorial",
        "n",
        Integer_t,
        If
          ( Op (Var "n", Equal, Val (Integer 0)),
            Val (Integer 1),
            Op
              ( Var "n",
                Mul,
                App (Var "factorial", Op (Var "n", Minus, Val (Integer 1))) ) ),
        Op (Val (Integer 5), Plus, App (Var "factorial", Val (Integer 5))) )
  in
  match type_check ast with
  | Some Integer_t -> assert_equal true true
  | _ -> assert_failure "5+factorial call should give integer"

(*
Type checking: 
(let function rec_fun x: Integer = 
  let x = (rec_function 5) + 3 in
  let y = (rec_function 3) AND true in
  x) in (rec_function 5)
  
  Should fail because you use rec_function as a Integer->Integer and
  Integer->Boolean at the same time
*)

let test_eval2 _ =
  let body =
    Let
      ( "x",
        Op (App (Var "rec_fun", Val (Integer 5)), Plus, Val (Integer 3)),
        Let
          ( "y",
            Op (App (Var "rec_fun", Val (Integer 3)), And, Val (Boolean true)),
            Var "x" ) )
  in
  let ast =
    LetFun
      ("rec_fun", "x", Integer_t, body, App (Var "rec_fun", Val (Integer 5)))
  in
  match type_check ast with
  | Some _ ->
      assert_failure
        "Should have failed AND and + are not of interoparable domain"
  | _ -> assert_equal true true

(*
Type checking: 
(let function rec_fun x: Integer = 
  let x = (rec_function 5) + 3 in
  let y = (rec_function 3) AND true in
  x) in (rec_function 5)
  
  Should fail because you use rec_function as a Integer->Integer and
  Integer->Boolean at the same time
*)

let test_eval3 _ =
  let body =
    Let
      ( "x",
        Op (App (Var "rec_fun", Val (Integer 5)), Equal, Val (Boolean false)),
        Let
          ( "y",
            Op (App (Var "rec_fun", Val (Integer 3)), And, Val (Boolean true)),
            Var "x" ) )
  in
  let ast =
    LetFun
      ("rec_fun", "x", Integer_t, body, App (Var "rec_fun", Val (Integer 5)))
  in
  match type_check ast with
  | Some _ -> assert_equal true true
  | _ -> assert_failure "rec_fun is Integer->Boolean why not?"

(* Test suite *)
let suite =
  "MiniFun Test Suite"
  >::: [
         "summing 5 and 4... " >:: test_eval0;
         "summing 5 and factorial(5).." >:: test_eval1;
         "using a type as an integer and boolean at same time..." >:: test_eval2;
         "using a type a..." >:: test_eval3;
       ]

(* Run the test suite *)
let () = run_test_tt_main suite
