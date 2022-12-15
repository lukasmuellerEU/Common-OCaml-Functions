(* //////////////////////////////
   basic functions
   ////////////////////////////// *)

(* "Function: max" : int -> int -> int *)
(* Returns the maximum of two int values *)
let max (x:int) (y:int) : int = if x >= y then x else y
    
(* "Function: mid" : int -> int -> int -> int *)
(* Returns the middle value of three int values *)
let mid (x:int) (y:int) (z:int) : int =
  if x <= y
  then if y <= z then y else z
  else if x <= z then x else z

(* //////////////////////////////
   helper rec funcions
   ////////////////////////////// *)

(* "Function: until" : ('a -> bool) -> ('a -> 'a) -> ('b -> 'b) -> 'a -> 'b -> 'b *) 
(* Applies the function f to a until the function p applied to s is true, then returns a *)
let rec until p n f s a =
  if p s then a
  else until p n f (n s) (f a) 
                               
(* "Function: sum" : (int -> int) -> int -> int *)
(* Returns the sum of the values returned by the function f when called on each integer from 1 to n *)
let rec sum (f:int -> int) (n:int) : int =
  if n < 1 then 0 else sum f (n-1) + f n 
                         
(* "Function: first" : (int -> bool) -> int -> int *)
(* Returns the first integer k for which the function f applied to k is true *)
let rec first f k =
  if f k then k 
  else first f ( k + 1 )

(* //////////////////////////////
   iter rec functions (helper function)
   ////////////////////////////// *)

(* "Function: iter" : tree -> int list -> tree *) 
let rec iter f n x =
  if n < 1 then x
  else iter f (n - 1) (f x)
      
(* "Function: iterdn" : tree -> int list -> tree *) 
let rec iterdn f n m s =
  if n < m then s else iterdn f (n-1) m (f(n,s)) 
                                                 
(* "Function: iterup" : tree -> int list -> tree *)   
let rec iterup f m n s =
  if m > n then s else iterup f (m+1) n (f(m,s))

(* //////////////////////////////
   basic functions with the help of advanced higher oder helper functions 
   ////////////////////////////// *)

(* "Function: power" : int -> int -> int *) 
(* Returns the result of raising x to the power of n *)
let power (x:int) (n:int) : int =
  iter (fun (a:int) : int -> a*x) n 1
  
(* "Function: sqrt" : int -> int *) 
 (* Returns the largest integer less than or equal to the square root of x *)
let sqrt (x:int) : int =
  first (fun (k:int) : bool -> k * k > x) 1 - 1

(* "Function: gauss" : int -> int *) 
(* Returns the sum of the integers from 1 to n using the Gauss formula (n * (n + 1) / 2) *)
let gauss (n:int) : int =
  let (i,res) = iter (fun ((i:int),(a:int)) : (int*int) -> (i+1, a+i)) n (1,0) in
  res

(* //////////////////////////////
   changed basic rec functions
   ////////////////////////////// *)

(* "Function: iterc" : ('a -> 'a) -> int -> 'a -> 'a*)    
(*Iter function with the help of the until function*)
let iterc f n s =
  until (fun s -> s < 1) (fun s -> s - 1) f n s

(* "Function: firstc" : ('a -> 'a) -> int -> 'a -> 'a*)    
(*First function with the help of the until function*)
let firstc p s =
  until p (fun s -> s + 1) (fun s -> s + 1) s s

(* //////////////////////////////
   lists functions
   ////////////////////////////// *) 

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

(* //////////////////////////////
   fold functions
   ////////////////////////////// *)


(* "Function: foldl" : '('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *) 
(* foldl is a function that applies a given function [f] to each element of a list [xl] and an accumulator [a],
 * starting from the left side of the list and moving towards the right. The function returns the final result
 * of applying [f] to the elements of the list and the accumulator.
 *
 * The function takes the following arguments:
 *   - f: the function to apply to each element of the list and the accumulator.
 *   - xl: the list of elements to apply the function [f] to.
 *   - a: the accumulator value.
*)
let rec foldl f xl a = match xl with
  | [] -> a
  | h::tl -> foldl f tl (f h a)
  
(* "Function: foldr" : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *) 
(* foldr applies a function to each element of a list, starting from the rightmost
   element, and combines the results with an accumulator value to produce a final
   result. *)
let rec foldr f xl a = match xl with
  | [] -> a
  | h::tl -> f h(foldr f tl a)

(* //////////////////////////////
   comparison and types
   ////////////////////////////// *)
   
(* This code defines a type 'comparison' that represents the possible results of
   comparing two values, and a function 'comp' that takes two values and returns
   their comparison. The 'comparison' type can take three values: 'LE' if the first
   value is less than the second, 'EQ' if they are equal, and 'GR' if the first
   value is greater than the second. The 'comp' function uses these values to
   compare its arguments and return the appropriate result. *)
(* "Type: comparison" : comparison = LE | EQ | GR *) 
type comparison = LE (* less *) (* x < y *)
                | EQ (* equal *) (* x = y *)
                | GR (* greater *) (* x > y *)

(* "Function: comp" : 'a -> 'a -> comparison *) 
let comp x y = if x = y then EQ else if x < y then LE else GR
                  
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

(* "Function: split" : 'a list -> 'a list * 'a list *) 
(* This function splits the list xs into two nearly equally large lists *)
let split xs = foldl (fun x (xs,ys) -> (ys, x::xs)) xs ([],[])

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

(* //////////////////////////////
   Trees
   ////////////////////////////// *)

   (* Type definition *)
type tree = T of tree list

(* Exmaple trees to test with *)
let t1 = T []
let t2 = T [t1; t1; t1]
let t3 = T [T[t2]; t1; t2] 

(* This function computes the size of a given tree 'T ts', which is defined as the number of nodes it contains (including the root).
It uses recursion to compute the size of each of the children of 'T ts' and returns the sum of those sizes plus 1 (to account for the root). *) 
(* "Function: size" : tree -> int *) 
let rec size (T ts) = foldl ( + ) (map size ts) 1 
   
   (* This function takes a tree 'T ts' and returns its arity, which is defined as the number of its children.
The function simply uses the 'length' function to count the number of elements in the list 'ts', which
represents the children of the tree 'T ts'. *)
(* "Function: arity" : tree -> int *) 
let arity (T ts) = length ts
    
(* "Function: succtree" : tree -> int -> tree *) 
(* Output: Successor tree at position n *)
let succtree (T ts) k = List.nth ts k    
  
(* "Function: isSubtree" : tree -> tree -> bool *) 
  (* This function checks if a given tree 't' is a subtree of another tree 'T ts'. It uses recursion to check
the root and the children of 'T ts'. If 't' is equal to 'T ts' or if 't' is a subtree of any of the children
of 'T ts', then the function returns true. Otherwise, it returns false. *)
let rec isSubtree t (T ts) =
  t = T ts || exists (isSubtree t) ts
  
(* "Function: countSubtree" : tree -> tree -> int *) 
(* This function counts the number of times a given tree 't' appears as a subtree of another tree 'T ts'.
It uses recursion to check the root and the children of 'T ts'. If 't' is equal to 'T ts', the function
returns 1. Otherwise, it counts the number of times 't' appears as a subtree of each of the children of
'T ts' and returns the sum of those counts. *)
let rec countSubtree t (T ts) =
  if t = T ts then 1 else foldl ( + ) (map (countSubtree t) ts) 0

(* "Function: compound" : tree -> bool*)
(* compound: tree -> bool
   Tests whether a tree is compound, that is, if it is a node with at least
   two subtrees.
*)
let compound (t: tree) : bool = match t with
  | T st -> List.length st >= 2 
                                
(* "Function: binary" : tree -> bool *)
(* Define a function that tests whether a tree is binary (i.e., has 0 or 2
   children). The function takes a tree as its argument and returns true if
   the tree is binary, and false otherwise. *)
let binary (T st) = match st with
    (* Pattern match on the list of children. If the list is empty, then the
     tree is a leaf and is therefore binary. If the list has one element,
     then the tree has one child and is therefore binary. If the list has
     two or more elements, then the tree has more than two children and is
     therefore not binary. *)
  | [] -> true
  | [_] -> true
  | _::_::_ -> false

(* "Function: balanced" : tree -> bool *)
(* Define a helper function that recursively checks whether a tree is
   balanced. A tree is balanced if:
   - it is a leaf, or
   - it has two children and both of its children are balanced. *)
let rec is_balanced (T st) = match st with
  | [] -> true (* A leaf is always balanced. *)
  | [_] -> false (* A tree with only one child is not balanced. *)
  | [left; right] -> (* A tree with two children is balanced if both children are balanced. *)
      is_balanced left && is_balanced right
  | _ -> false (* A tree with more than two children is not balanced. *)

(* Define the balanced function. This is just a wrapper around the
   is_balanced function that defines the type of its argument. *)
let balanced t = is_balanced t
  

(* "Function: subtree" : tree -> int list -> tree *) 
(* Output: Subtree at address ad *)
let rec subtree t ad = match ad with
  | [] -> t
  | h::tl -> subtree (succtree t h) tl 
    
(* "Function: bbtree" : int -> tree *) 
(* This function returns a balanced binary tree with 'n' nodes. If 'n' is negative, it raises an
exception. If 'n' is 0 or 1, the function returns a tree with no or one node, respectively. Otherwise,
it returns a tree with 'n' nodes that is balanced in the sense that the left and right subtrees have the same number of nodes. *)
let bbtree n =
  if n < 0 then failwith "bbtree: negative argument"
  else if n = 0 then T []
  else iter (fun (T ts) -> T (T ts :: (T [] :: ts))) n (T [])
  (*
//                              //
// End of pre-defined functions //
//                              //
*)