open Alcotest
open Minityfun 
open Minityfun.TyFunAst

let test_arithmetic_precedence () = 
  let expected_output = Integer(11) in
  let prog_str = "4 + 3 * 2" in
  match parse_with_errors (Lexing.from_string prog_str) with
  | Some prog_ast -> 
    begin
    let value =  eval prog_ast in
    if (value <> expected_output) then
      Alcotest.fail "Precedence of * is not given"
    else
      ()
    end
  | None -> Alcotest.fail "input in test_arithmetic_precedence is ill-formed"
let () = run "Minityfun tests" [
  "Parsing Minityfun",
  [test_case "test_arithmetic_precedence" `Quick test_arithmetic_precedence;]
]
