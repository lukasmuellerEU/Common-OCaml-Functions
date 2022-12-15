
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
