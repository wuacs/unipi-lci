(* A FSA (Finite State Automata) using maps for transitions *)

exception InitialStateNotProvided

type fsa = {initial: bool; accepting: bool; transitions: (char, (fsa ref)) Hashtbl.t }


(* Creates a list of char from a string preserving the order in index 0 there is the last character*)
let explode str = 
  let rec helper out idx =
    if idx >= String.length str || idx < 0 then
        out
    else begin
      helper (str.[idx]::out) (idx+1)
    end
    in 
    List.rev (helper [] 0)


let check str fsa = 
  if !(fsa).initial = false then
    raise InitialStateNotProvided
  else
  let rec helper list fsa = 
    match !fsa with
    | {initial = x; accepting = y; transitions = t} -> 
      match list with
      | [] -> y 
      | l::ls -> match Hashtbl.find_opt t l with
                      | None -> false
                      | Some(binding) -> (helper ls (binding))
    in helper (List.rev(explode str)) fsa

let () = 
let q0 = ref ({accepting = true; initial = true; transitions = Hashtbl.create 100})
in
let q1 = ref ({accepting = true; initial = false; transitions = Hashtbl.create 100})
in
let q2 = ref ({accepting = false; initial = false; transitions = Hashtbl.create 100})
in
let q3 = ref ({accepting = false; initial = false; transitions = Hashtbl.create 100})
in 
let q4 = ref ({accepting = false; initial = false; transitions = Hashtbl.create 100})
in 
let q5 = ref ({accepting = true; initial = false; transitions = Hashtbl.create 100})
in
Hashtbl.add ((!q0).transitions) '0' q1;
Hashtbl.add ((!q0).transitions) '1' q2;
Hashtbl.add ((!q1).transitions) '0' q0;
Hashtbl.add ((!q1).transitions) '1' q4;
Hashtbl.add ((!q2).transitions) '0' q3;
Hashtbl.add ((!q2).transitions) '1' q0;
Hashtbl.add ((!q3).transitions) '0' q2;
Hashtbl.add ((!q3).transitions) '1' q5;
Hashtbl.add ((!q5).transitions) '0' q4;
Hashtbl.add ((!q5).transitions) '1' q3;
Hashtbl.add ((!q4).transitions) '0' q5;
Hashtbl.add ((!q4).transitions) '1' q1;
Printf.fprintf stdout "is it divisible by 3? %B\n" (check "100001110" q0)  