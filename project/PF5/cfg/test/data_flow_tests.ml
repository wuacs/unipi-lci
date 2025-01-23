open Alcotest

let test_defined_analysis () =
  let input_dir = Sys.getcwd() ^ "/../../../programs/" in
  let output_dir = Sys.getcwd() ^ "/../../../test/defined_analysis/expected/" in
  Sys.readdir input_dir
  |> Array.to_list
  |> List.iter (fun input_file ->
      let input_channel = open_in (input_dir ^ input_file) in
      let expected_channel = open_in (output_dir ^ (Filename.chop_extension input_file) ^ ".out") in
      let actual_output = Lexing.from_channel input_channel in
      match Miniimp.parse_with_errors actual_output with
      | None -> Alcotest.fail "Error in parsing MiniImp in Defined Analysis testing"
      | Some prog ->
        (* Read the expected output (assuming it's a single line containing "true" or "false") *)
        (try
           let expected_output = input_line expected_channel in
           close_in expected_channel;  (* Close the file after reading *)
           close_in input_channel;     (* Close the input file as well *)
           let expected_bool = bool_of_string expected_output in
           let actual_bool = Data_flow_analysis.check_for_undefinedness
             (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog)) in
           Alcotest.check Alcotest.bool input_file expected_bool actual_bool
         with End_of_file ->
           close_in expected_channel;
           close_in input_channel;
           Alcotest.fail ("File " ^ input_file ^ " does not contain a boolean for defined analysis")))

let () = run "Data Flow Tests" [
  "Defined Analysis Tests", [test_case "test_defined_analysis" `Quick test_defined_analysis]
]