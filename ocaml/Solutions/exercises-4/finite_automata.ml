type dfa =  State of {accepting: bool; transitions: dfa ref array} |
            Null 

let explode str = 
  let rec helper out idx =
    if idx >= String.length str || idx < 0 then
        out
    else begin
      helper (str.[idx]::out) (idx+1)
    end
    in 
    helper [] 0

let check dfa str = 
  let arr_str = explode str;
in
  let rec helper dfa chars = 
    (match !dfa with
    | State({accepting = x; transitions = transitions}) -> 
          (match chars with
            | [] -> x
            | x::xs -> let code = (Char.code x) in 
                      if code >= Array.length transitions || !(transitions.(code))=Null then
                          false
                      else
                          helper transitions.(code) xs)
    | Null -> false)
in
  helper dfa arr_str

let () = 
let q0 = ref (State({accepting = true; transitions =  Array.make 256 (ref Null)}))
in
let q1 = ref (State({accepting = true; transitions =  Array.make 256 (ref Null)})) 
in
let q2 = ref (State({accepting = false; transitions =  Array.make 256 (ref Null)})) 
in
let q3 = ref (State({accepting = false; transitions =  Array.make 256 (ref Null)})) 
in 
let q4 = ref (State({accepting = false; transitions =  Array.make 256 (ref Null)})) 
in 
let q5 = ref (State({accepting = false; transitions =  Array.make 256 (ref Null)}))
in
(match !q0 with 
  | State { transitions = transitions} -> transitions.(Char.code '0') <- q1; transitions.(Char.code '1') <- q2;
  | Null -> failwith "State q0 is null");
(match !q1 with
| State { transitions = transitions } -> transitions.(Char.code '1') <- q4; transitions.(Char.code '0') <- q0;
| Null -> failwith "State q1 is null");
(match !q2 with
| State { transitions = transitions } -> transitions.(Char.code '1') <- q0; transitions.(Char.code '0') <- q3;
| Null -> failwith "State q2 is null");
(match !q3 with
| State { transitions = transitions } -> transitions.(Char.code '1') <- q5; transitions.(Char.code '0') <- q2;
| Null -> failwith "State q3 is null");
(match !q5 with
| State { transitions = transitions } -> transitions.(Char.code '1') <- q3; transitions.(Char.code '0') <- q4;
| Null -> failwith "State q5 is null");
(match !q4 with
| State { transitions = transitions } -> transitions.(Char.code '1') <- q1; transitions.(Char.code '0') <- q5;
| Null -> failwith "State q4 is null");
Printf.fprintf stdout "is it divisible by 3? %B" (check q0 "10111001")  