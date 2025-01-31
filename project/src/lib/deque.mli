type 'a elem = { mutable l1 : 'a elem; mutable l2 : 'a elem; v : 'a option }
type 'a queue = { mutable front : 'a elem; mutable back : 'a elem }

val init : unit -> 'a queue
val is_empty : 'a queue -> bool
val put_between : 'a elem -> 'a elem -> 'a -> unit
val push_front : 'a -> 'a queue -> unit
val push_back : 'a -> 'a queue -> unit
val is_back : 'a elem -> 'a queue -> bool
val is_front : 'a elem -> 'a queue -> bool
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a queue -> 'b
val combine : 'a queue -> 'b queue -> ('a * 'b) queue
