open Alcotest

let fibonacci n = 
  let rec helper v1 v2 n = 
    if n <= 2 then v2 else helper v2 (v1 + v2) (n-1)
  in
    helper 1 1 n

let factorial n = 
  let rec help n t = 
    if n < 2 then t else help (n-1) (n*t)
  in
    help n 1

let sum_first_naturals n = 
  n * (n+1) / 2

let power2 n = 
  let rec helper t v = 
    if t < 1 then v else helper (t-1) v*2
  in
    helper n 1 


let test_fibonacci () =
  let tcs = 10 in
  let filename = "fibonacci.miniimp" in
  let input_dir = Sys.getcwd() ^ "/../../../programs/" in
  let input_channel = open_in (input_dir ^ filename) in
  let actual_output = Lexing.from_channel input_channel in
    match Miniimp.parse_with_errors actual_output with
    | None -> Alcotest.fail "Error in parsing MiniImp's Fibonacci, target code testing"
    | Some prog ->
        (try
            let fibonacci_minirisc = Target_code.eval ((Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog))) in
            let inputs = List.init tcs (fun x -> x) in
            close_in input_channel;     (* Close the input file as well *)
            List.iter (fun x -> Alcotest.check Alcotest.int ("error fibonacci for " ^string_of_int x) (fibonacci x) (fibonacci_minirisc 4 x)) 
                inputs
          with End_of_file ->
            close_in input_channel;
            Alcotest.fail ("Fibonacci broken"))
let test_factorial () = 
  let tcs = 10 in
  let filename = "factorial.miniimp" in
  let input_dir = Sys.getcwd() ^ "/../../../programs/" in
  let input_channel = open_in (input_dir ^ filename) in
  let actual_output = Lexing.from_channel input_channel in
    match Miniimp.parse_with_errors actual_output with
    | None -> Alcotest.fail "Error in parsing MiniImp's Factorial, target code testing"
    | Some prog ->
        (try
            let factorial_minirisc = Target_code.eval ((Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog))) in
            let inputs = List.init tcs (fun x -> x) in
            close_in input_channel;     (* Close the input file as well *)
            List.iter (fun x -> Alcotest.check Alcotest.int ("error factorial for " ^ string_of_int x) (factorial x) (factorial_minirisc 4 x)) 
                inputs
          with End_of_file ->
            close_in input_channel;
            Alcotest.fail ("Factorial broken"))
  
let test_sumnaturals () =
  let tcs = 100 in
  let filename = "sumnaturals.miniimp" in
  let input_dir = Sys.getcwd() ^ "/../../../programs/" in
  let input_channel = open_in (input_dir ^ filename) in
  let actual_output = Lexing.from_channel input_channel in
    match Miniimp.parse_with_errors actual_output with
    | None -> Alcotest.fail "Error in parsing MiniImp's Sum of N naturals, target code testing"
    | Some prog ->
        (try
            let factorial_minirisc = Target_code.eval ((Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog))) in
            let inputs = List.init tcs (fun x -> x) in
            close_in input_channel;     (* Close the input file as well *)
            List.iter (fun x -> Alcotest.check Alcotest.int ("error natural sum for " ^ string_of_int x) (sum_first_naturals x) (factorial_minirisc 5 x)) 
                inputs
          with End_of_file ->
            close_in input_channel;
            Alcotest.fail ("Sum broken"))

let test_power2 () =
  let tcs = 20 in
  let filename = "power2.miniimp" in
  let input_dir = Sys.getcwd() ^ "/../../../programs/" in
  let input_channel = open_in (input_dir ^ filename) in
  let actual_output = Lexing.from_channel input_channel in
    match Miniimp.parse_with_errors actual_output with
    | None -> Alcotest.fail "Error in parsing MiniImp's power of 2 program, target code testing"
    | Some prog ->
        (try
            let power2_minirisc = Target_code.eval ((Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog))) in
            let inputs = List.init tcs (fun x -> x) in
            close_in input_channel; (* Close the input file as well *)
            List.iter (fun x -> 
              Alcotest.check 
              Alcotest.int 
              (Printf.sprintf "error computing 2^%d" x) 
              (power2 x) 
              (power2_minirisc 4 x)) inputs
          with End_of_file ->
            close_in input_channel;
            Alcotest.fail ("Sum broken"))

let () = run "Fibonacci in MiniRISC test" [
  "Target Code Tests",
  [test_case "test_fibonacci" `Quick test_fibonacci;
  test_case "test_factorial" `Quick test_factorial;
  test_case "test_sumnaturals" `Quick test_sumnaturals;
  test_case "test_power2" `Quick test_power2]
]