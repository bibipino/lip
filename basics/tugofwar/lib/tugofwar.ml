(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let toklist_of_string s = 
  let chlist = explode s in
  List.map (fun x -> match x with
  | 'A' -> A
  | 'B' -> B
  | '=' -> X
  | _ -> failwith "non valid input") chlist
;;

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec validtokens prev list =
    match list with
    | [] -> true
    | t::h when prev = A -> if t=A || t=X then validtokens t h else false
    | t::h when prev = X -> if t=B then validtokens t h else false
    | t::h when prev = B -> if t=B then validtokens t h else false
    | _ -> false
in
  validtokens A l
;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let sum = List.fold_right (+) (List.map (fun x -> match x with
  | A -> 1
  | X -> 0
  | B -> -1) l) 0
in
  if sum > 0 then A else if sum = 0 then X else B
;;

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
| A -> "Ha vinto la squadra A!"
| X -> "Nessuno ha vinto. Pareggio."
| B -> "Ha vinto la squadra B!"
;;

