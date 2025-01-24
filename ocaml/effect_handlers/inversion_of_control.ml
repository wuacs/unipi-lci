open Effect
open Effect.Deep
open Printf

let inverted = Fun.flip List.iter [1;2;3] 
let invert (type a) ~(iter: (a -> unit) -> unit) : a Seq.t =
    let module M = struct
      type _ Effect.t += Yield : a -> unit Effect.t
    end in
    let yield y = perform (M.Yield y) in
    fun () -> match iter yield with
    | () -> Seq.Nil
    | effect M.Yield v, k -> Seq.Cons(v, continue k)
let sseq = (invert ~iter:(inverted))
let next = Seq.to_dispenser sseq