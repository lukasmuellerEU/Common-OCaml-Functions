
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
  