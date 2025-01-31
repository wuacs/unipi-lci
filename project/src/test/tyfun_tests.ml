open Minityfun
open Minityfun.TyFunAst

let test_arithmetic_precedence () =
  let prog_str = "4 + 3 * 2" in
  match parse_with_errors (Lexing.from_string prog_str) with
  | Some prog_ast -> (
      let value = eval prog_ast in
      match value with
      | Integer x ->
          if x <> 10 then Alcotest.fail "Precedence of * is not given"
          else
            Alcotest.check Alcotest.bool "Precedence of * is kept in tyfun "
              true true
      | _ -> Alcotest.fail "Unexpected value type")
  | None -> Alcotest.fail "Input in test_arithmetic_precedence is ill-formed"

let () =
  Alcotest.run "Minityfun tests"
    [
      ( "Parsing Minityfun",
        [
          Alcotest.test_case "test_arithmetic_precedence" `Quick
            test_arithmetic_precedence;
        ] );
    ]
