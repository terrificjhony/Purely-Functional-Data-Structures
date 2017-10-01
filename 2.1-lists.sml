signature STACK =
sig
    type 'a Stack
	    
    val empty : 'a Stack
    val isEmpty : 'a Stack -> bool
    val cons : 'a * 'a Stack -> 'a Stack
    val head : 'a Stack -> 'a
    val tail : 'a Stack -> 'a Stack
end

(* implementation of stacks using the build-in type of lists *)
structure List :>STACK =
struct
type 'a Stack = 'a list

val empty = []
fun isEmpty s = null s
fun cons (x,s) = x :: s
fun head s = hd s
fun tail s =tl s
end

    (* custom stack *)
structure CustomStack :>STACK =
struct
datatype 'a Stack = NIL | Cons of 'a * 'a Stack

val empty = NIL
fun isEmpty (xs: 'a Stack)=
  case xs of
      NIL => true
    | _ => false
	       

fun cons(x,s) = Cons(x,s)
exception EMPTY
fun head NIL = raise EMPTY
  | head (Cons (x,s)) = x

fun tail NIL = raise EMPTY
  | tail (Cons (x,s)) = s
end


(* cons two lists *)
(*fun [] ++ ys = ys
 |  (x::xs) ++ ys = x :: (xs++ys)*)

fun ++ (xs,ys) =
  case xs of
      [] => ys
   | x :: xs' => x :: (++ (xs',ys))
			     
