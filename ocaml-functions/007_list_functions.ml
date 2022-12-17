
(* //////////////////////////////
   list functions
   ////////////////////////////// *) 

(* "Function: tl" : 'a list -> 'a list *)
(* Returns the tail of the given list, which is all the elements except for the first one.
   If the list is empty, raises the Not_found exception. *)
let tl xl = match xl with
| [] -> raise Not_found
| h::tl -> tl

(* "Function: hd" : 'a list -> 'a *) 
(* Returns the head of the given list, which is the first element.
   If the list is empty, raises the Not_found exception. *)
let hd xl = match xl with
| [] -> raise Not_found
| h::tl -> h

(* "Function: length" : 'a list -> int *) 
(* Returns the length of the given list xl *) 
let rec length xl = match xl with
  | [] -> 0 
  | h::tl -> 1 + length tl

(* "Function: rev" : 'a list -> 'a list *) 
(* Reverses the given list xl and returns the result *)
let rec rev xl = match xl with
  | [] -> []
  | h::tl -> rev tl @ [h]

(* "Function: concat" : 'a list -> 'a list -> 'a list *) 
(* Concatenates the given lists xl and yl and returns the result *)
let rec concat xl yl = match xl with
  | [] -> yl
  | h::tl -> h :: concat tl yl

               
(* "Function: flatten" : 'a list list -> 'a list *) 
(* Returns a new list that is the result of flattening the given list of lists xl *)
let rec flatten xl = match xl with
  | [] -> []
  | h::tl -> h @ flatten tl

(* "Function: map" : ('a -> 'b) -> 'a list -> 'b list *) 
(* Applies a given function to each element in a list and returns a new list of the results. *)
let rec map f xl = match xl with
  | [] -> []
  | h::tl -> f h :: map f tl

(* "Function: filter" : ('a -> bool) -> 'a list -> 'a list *) 
(* Returns a new list containing only elements of the original list that satisfy a given predicate. *)
let rec filter p xl = match xl with
  | [] -> []
  | h::tl -> if p h then h :: filter p tl else filter p tl

(* "Function: exists" : ('a -> bool) -> 'a list -> bool *) 
(* Returns true if at least one element in the list satisfies a given predicate, and false otherwise. *)
let rec exists p xl = match xl with
  | [] -> false
  | h::tl -> p h || exists p tl

(* "Function: forall" : ('a -> bool) -> 'a list -> bool *) 
(* Returns true if all elements in the list satisfy a given predicate, and false otherwise. *)
let rec forall p xl = match xl with
  | [] -> true
  | h::tl -> p h && forall p tl 

(* "Function: split" : 'a list -> 'a list * 'a list *) 
(* Call exmaple: split [1; 2; 3; 4] *)
(* This function splits the list xs into two nearly equally large lists *)
let split xs = foldl (fun x (xs,ys) -> (ys, x::xs)) xs ([],[])

(* "Function: combine" : 'a list -> 'b list -> ('a * 'b) list *) 
(* Returns a list of tuples containing every possible combination of elements from the input lists xs and ys. *)
(* Example call "combine [1;2] [3;4]"" -> [(1, 3); (1, 4); (2, 3); (2, 4)] *)
let combine xs ys =
  flatten (foldr (fun x y -> (map (fun b -> (x,b)) ys) :: y) xs [])

(* "Function: cross" : ('a * 'b -> bool) -> 'a list -> 'b list -> ('a * 'b) list *) 
(* Returns a list of tuples containing every possible combination of elements from the input lists xs and ys that satisfy the predicate p. *)
let cross p xs ys =
  filter p (combine xs ys)
