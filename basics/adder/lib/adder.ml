(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l = 
  let rec adl = function
  | [] -> 0
  | t::h -> t + adl h
in
  adl l
;; (* replace 0 with actual code *)
