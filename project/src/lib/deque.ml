type 'a elem = { mutable l1 : 'a elem; mutable l2 : 'a elem; v : 'a option }
type 'a queue = { mutable front : 'a elem; mutable back : 'a elem }

let init () =
  let rec guard_left = { l1 = guard_left; l2 = guard_right; v = None }
  and guard_right = { l1 = guard_left; l2 = guard_right; v = None } in
  { front = guard_left; back = guard_right }

let is_empty q =
  let f = q.front and b = q.back in
  f.l2 == b

let put_between p q x =
  let r = { l1 = p; l2 = q; v = Some x } in
  if p.l1 == q then p.l1 <- r else p.l2 <- r;
  if q.l1 == p then q.l1 <- r else q.l2 <- r

let push_front x q = put_between q.front q.front.l2 x
let push_back x q = put_between q.back q.back.l1 x
let is_back b q = if b.l2 == q.back then true else false
let is_front t q = if t.l1 == q.front then true else false

let fold_left f acc q =
  let x = ref q.front in
  let acc = ref acc in
  while is_back !x q <> true do
    match !x.v with
    | Some el -> acc := f !acc el
    | _ ->
        ();
        x := !x.l2
  done;
  !acc

(** Analogous to {!List.combine} *)
let combine q1 q2 =
  let x = ref q1.front in
  let y = ref q2.front in
  let combined = init () in
  while (is_front !x q1 || is_front !y q2) <> true do
    match (!x.v, !y.v) with
    | Some v1, Some v2 -> push_front (v1, v2) combined
    | _ -> raise (Invalid_argument "Queues of different length")
  done;
  combined
