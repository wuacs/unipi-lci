open Alcotest

let fibonacci n = 
  let rec helper v1 v2 n = 
    if n <= 1 then v2 else helper v2 (v1 + v2) (n-1)
  in
    helper 1 0 n

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

let compute_and_test ~filename ~oracle ~tcs =
  let input_dir = Sys.getcwd() ^ "/../../../miniimp_programs/" in
  let inputs = List.init tcs (fun x -> x) in
  List.iter (fun x ->
    Alcotest.check
      Alcotest.int
      (Printf.sprintf "Error in %s for input %d" filename x)
      (oracle x)
      (Target_code.compile_and_run_imp_from_file ~register_number:4 (input_dir ^ filename) ~input:x ~check_undefinedness:true)
  ) inputs

let test_fibonacci () =
  compute_and_test ~filename:"fibonacci.miniimp" ~oracle:fibonacci ~tcs:10

let test_factorial () =
  compute_and_test ~filename:"factorial.miniimp" ~oracle:factorial ~tcs:10

let test_sumnaturals () =
  compute_and_test ~filename:"sumnaturals.miniimp" ~oracle:sum_first_naturals ~tcs:100

let test_power2 () =
  compute_and_test ~filename:"power2.miniimp" ~oracle:power2 ~tcs:20

let () = run "Fibonacci in MiniRISC test" [
  "Target Code Tests",
  [test_case "test_fibonacci" `Quick test_fibonacci;
   test_case "test_factorial" `Quick test_factorial;
   test_case "test_sumnaturals" `Quick test_sumnaturals;
   test_case "test_power2" `Quick test_power2]
]
