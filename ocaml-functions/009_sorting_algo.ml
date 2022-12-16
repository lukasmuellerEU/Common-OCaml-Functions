                  
(* //////////////////////////////
   sorting algorithms
   ////////////////////////////// *)
   
(* // insertion sort \\*)

(* "Function: insert" : 'a -> 'a list -> 'a list *)
(* insert adds an element 'x' to a list 'yl' in the correct position so that the
   resulting list is sorted in ascending order. The function uses a recursive
   approach, where it compares the element 'x' to the first element 'h' of the
   list 'yl' and inserts it either before or after 'h' depending on whether 'x'
   is less than or greater than 'h'. It then calls itself recursively on the
   remaining elements of the list 'tl' to insert 'x' in the correct position
   within the sublist. If the list 'yl' is empty, it simply returns a new list
   containing only 'x'. *)
let rec insert x yl = match yl with
  | [] -> [x]
  | h::tl -> if x > h
      then h :: insert x tl
      else x :: h :: tl

(* "Function: isort" : 'a list -> 'a list *) 
(* isort sorts a list 'xs' in ascending order using the 'foldl' and 'insert'
   functions. It applies 'insert' to each element of the list, starting from
   the leftmost element, and combines the results to produce a final, sorted
   list. *)
let isort xs = foldl insert xs []

(* "Function: insertcom" : ('a -> 'a -> comparison) -> 'a -> 'a list -> 'a list *) 
(* insertcomp comp x yl inserts x into yl in the correct order according to the comparison function comp. *)
let rec insertcomp comp x yl = match yl with
  | [] -> [x]
  | h::tl -> if comp x h = GR
      then h :: insertcomp comp x tl
      else x :: h :: tl

(* "Function: cisort" : ('a -> 'a -> comparison) -> 'a list -> 'a list  *) 
(* cisort comp xs returns a sorted list with the elements from xs according to the comparison function comp. *)
let cisort comp xs = foldl (insertcomp comp) xs []

(* // Lexicographic Sorting \\ *)

(* "Function: lex" : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison *) 
(* lex comp xl yl compares xl and yl lexicographically according to the comparison function comp. *)
let rec lex (comp : 'a -> 'a -> comparison) xl yl = match xl, yl with
  | [] , [] -> EQ
  | [] , _::_ -> LE
  | _::_, [] -> GR
  | xh::xtl, yh::ytl -> match (comp xh yh) with
    | EQ -> lex comp xtl ytl
    | v -> v

  
(* "Function: lexsort" : 'a list list -> 'a list list *)
(* lexsort xl returns a sorted list with the elements from xl in lexicographical order. *)
let lexsort xl = cisort (lex comp) xl

(* // Mergesort \\ *)

(* "Function: merge" : 'a list -> 'a list -> 'a list *) 
(* merge xl yl merges the elements from xl and yl into a single sorted list. *)
let rec merge xl yl = match xl, yl with
  | [], yl -> yl
  | xl, [] -> xl
  | xh::xtl, yh::ytl -> if xh > yh
      then yh :: merge xl ytl
      else xh :: merge xtl yl

(* "Function: msort" : 'a list -> 'a list *) 
(* msort xl sorts the list xl using the merge sort algorithm which is implemented by the function merge *) 
let rec msort xl = match xl with
  | [] -> []
  | [x] -> [x]
  | xl -> let (xs,ys) = split xl in
      merge (msort xs) (msort ys)
