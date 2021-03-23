type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* Part 1 *)

fun maxargs (CompoundStm (s1, s2)) = Int.max (maxargs s1, maxargs s2)
  | maxargs (AssignStm   ( _,  e)) = maxargsExp e
  | maxargs (PrintStm    es      ) =
    Int.max (length es, foldl Int.max 0 (map maxargsExp es))

and maxargsExp (OpExp   (e1, _, e2)) = Int.max (maxargsExp e1, maxargsExp e2)
  | maxargsExp (EseqExp ( s, e)    ) = Int.max (maxargs     s, maxargsExp  e)
  | maxargsExp _                     = 0

(* Part 2 *)

fun update (t, i, v) = (i, v) :: t

fun lookup (         [], _) = raise Empty
  | lookup ((i, v) :: t, x) = if i = x then v else lookup (t, x)

fun interp s = ignore (interpStm (s, []))

and interpStm (CompoundStm (s1, s2), t) = interpStm (s2, interpStm (s1, t))
  | interpStm (AssignStm   ( i,  e), t) =
    let val (v, t') = interpExp (e, t)
    in update(t', i, v)
    end
  | interpStm (PrintStm      [], t) = (print "\n"; t)
  | interpStm (PrintStm (e::es), t) =
    let val (v, t') = interpExp (e, t)
    in (print (Int.toString v ^ (if null es then "" else " "));
	interpStm (PrintStm es, t'))
    end

and interpExp (IdExp            i, t) = (lookup (t, i), t)
  | interpExp (NumExp           v, t) = (v, t)
  | interpExp (OpExp  (e1, b, e2), t) =
    let val (v1, t' ) = interpExp (e1, t );
	val (v2, t'') = interpExp (e2, t')
    in (evalOpExp (v1, b, v2), t'')
    end
  | interpExp (EseqExp (s, e), t) = interpExp (e, interpStm (s, t))

and evalOpExp (v1,  Plus, v2) = v1 + v2
  | evalOpExp (v1, Minus, v2) = v1 - v2
  | evalOpExp (v1, Times, v2) = v1 * v2
  | evalOpExp (v1,   Div, v2) = v1 div v2
