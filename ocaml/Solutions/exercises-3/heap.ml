type 'a t =
| Leaf
| Node of ('a * int) * 'a t ref * 'a t ref

exception EmptyHeap

let empty = Leaf

let singleton a = Node((a, 1), ref Leaf, ref Leaf)

let get_min = function
    | Node((x, t),_,_) -> x
    | Leaf -> raise EmptyHeap

let s_value = function
  | Node((x, t),_,_) -> t 
  | _ -> 0 

let merge heap1 heap2 = 
  let rec helper newheap heap1 heap2 =
    match (!heap1, !heap2) with
    | (Leaf, Leaf) -> ()
    | (Leaf, _) -> newheap := !heap2
    | (_, Leaf) -> newheap := !heap1
    | (Node((v, s), y, t), Node((v', s'), y', t')) -> 
    if v < v' then begin
      let merged = ref Leaf in 
      helper merged t heap2;
      if (s_value !y) > (s_value !merged) then
          newheap := (Node((v, 1+(s_value !merged)), y, merged))
      else 
          newheap := (Node((v, 1+(s_value !y)), merged, y))
    end else begin 
      let merged = ref Leaf in 
      helper merged t' heap1;
      if (s_value !y') > (s_value !merged) then
          newheap := (Node((v', 1+s_value !merged), y', merged))
      else 
          newheap := (Node((v', 1+(s_value !y')), merged, y'))
      end
    in                           
    let merge = (ref Leaf) 
    in                                
    helper merge heap1 heap2;
    !merge

let insert k h = merge (ref h) (ref(singleton k))

let delete min = function
| Leaf -> Leaf
| Node((_,_),x,y) -> merge x y


(* Example of usage *)
let () =
  let h1 = insert 5 empty in
  let h2 = insert 10 h1 in
  let h3 = insert 3 h2 in
  let h4 = insert 8 h3 in
  (* Now h4 is a heap with elements 3, 5, 8, and 10 *)

  (* Get minimum value *)
  let min_value = get_min h4 in
  Printf.printf "Minimum value: %d\n" min_value;  (* Should print 3 *)

  (* Delete the minimum value *)
  let new_heap = delete min_value h4 in

  (* Get new minimum value *)
  let new_min_value = get_min new_heap in
  Printf.printf "New minimum value: %d\n" new_min_value;  (* Should print 5 *)
