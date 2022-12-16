
(* //////////////////////////////
   changed basic rec functions
   ////////////////////////////// *)

(* "Function: iterwuntil" : ('a -> 'a) -> int -> 'a -> 'a*)    
(*Iter function with the help of the until function*)
let iterwuntil f n s =
  until (fun s -> s < 1) (fun s -> s - 1) f n s

(* "Function: firstwuntil" : ('a -> 'a) -> int -> 'a -> 'a*)    
(*First function with the help of the until function*)
let firstwuntil p s =
  until p (fun s -> s + 1) (fun s -> s + 1) s s
