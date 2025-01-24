open Effect
open Effect.Deep

type _ Effect.t += 
  | Fork : (unit -> unit) -> unit t
  | Yield : unit t 
  | Xchng : int -> int t
let fork f = perform (Fork f) 
let yield () = perform Yield
let xchg i = perform (Xchng i)
let run (main : unit -> unit) : unit = 
  let exchanger : (int * (int, unit) continuation) option ref = ref None in
  let queue = Queue.create () in
  let enqueue c v = 
    let task () = continue c v in
    Queue.push task queue in
  let dequeue () = if Queue.is_empty queue then () else let t = Queue.pop queue in t() in
  let rec spawn f : unit = 
    match f () with
    | () -> dequeue ()
    | effect Yield, k -> enqueue k (); dequeue ()
    | effect (Fork f), t -> enqueue t (); spawn f;
    | effect (Xchng i), k -> 
      match (!exchanger) with
      | Some (exchi, t) -> exchanger := None; (enqueue k exchi); continue t i; 
      | None -> exchanger := Some(i, k); dequeue();
  in 
  spawn main

open Printf

let _ = run (fun _ ->
  fork (fun _ -> printf "[Thread 1] active wants to give integer 1 to someone...\n";
  let t = xchg 1 in
  printf "[Thread 1] Someone gave me %d \n" t
  );
  fork (fun _ -> printf "[Thread 2] active wants to give integer 2 to someone...\n";
  let t = xchg 2 in
  printf "[Thread 2] Someone gave me %d\n" t
  ) )

