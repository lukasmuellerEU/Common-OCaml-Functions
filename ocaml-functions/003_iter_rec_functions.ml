
(* //////////////////////////////
   iter rec functions (helper function)
   ////////////////////////////// *)

(* "Function: iter" : ('a -> 'a) -> int -> 'a -> 'a *) 
(* Applies the given function f to the value x a total of n times. *)
let rec iter f n x =
  if n < 1 then x
  else iter f (n - 1) (f x)
      
(* "Function: iterdn" : (int * 'a -> 'a) -> int -> int -> 'a -> 'a *) 
(* Applies the given function f to the value s a total of n-m+1 times,
   starting with the value of n and decrementing n by 1 each time. *)
let rec iterdn f n m s =
  if n < m then s else iterdn f (n-1) m (f(n,s)) 
                                                 
(* "Function: iterup" : (int * 'a -> 'a) -> int -> int -> 'a -> 'a *)   
(* Applies the given function f to the value s a total of n-m+1 times,
   starting with the value of m and incrementing m by 1 each time. *)
let rec iterup f m n s =
  if m > n then s else iterup f (m+1) n (f(m,s))