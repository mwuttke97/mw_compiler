(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



structure Compiler :> COMPILER = struct

fun compE f (W.Add (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.ADD]
  | compE f (W.Mul (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.MUL]
  | compE f (W.Sub (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.SUB]
  | compE f (W.Leq (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.LEQ]
  | compE f (W.Geq (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.GEQ]
  | compE f (W.Lt  (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.LT]
  | compE f (W.Gt  (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.GT]
  | compE f (W.Eq  (e1, e2)) = compE f e2 @ compE f e1 @ [Machine.EQ]
  | compE f (W.Var x       ) = [Machine.GET (Env.lookup f x)]
  | compE f (W.Con x       ) = [Machine.CON x]

fun compS f (W.Asig (x, exp))    = compE f exp @ [Machine.PUT (Env.lookup f x)]
  | compS f (W.If (exp, s1, s2)) =
  	let
  	  val ce  = compE f exp
	  val cs1 = compS f s1
	  val cs2 = compS f s2
	in
	  ce @ [Machine.CBRAN (List.length cs1 + 2)] @ cs1 @ [Machine.BRAN (List.length ce + 1)] @ cs2
	end
  | compS f (W.While (exp, s)) =
  	let
	  val ce = compE f exp
	  val cs = compS f s
	in
	  ce @ [Machine.CBRAN (List.length cs + 2)] @ cs @ [Machine.BRAN (~(List.length cs + List.length ce + 1))]
	end
  | compS f (W.SCon (s1, s2)) = compS f s1 @ compS f s2
  | compS f (W.Nop) = nil


fun compD ds =
	let
	  val (f, _, revcode) = foldl (fn ((x,y):W.declaration, (f, counter, code)) =>
	  	(Env.update f x (counter), counter+1, (compE f y) :: code)) (Env.empty, 0, nil) ds
	in
	  (f, List.concat (rev revcode))
	end

fun clear 0 = [Machine.HALT]
  | clear n = (Machine.PUT (n-1)) :: clear (n-1)

fun compR f n exp = compE f exp @ clear n

fun compile' (ds, s, r) =
	let
	  val (f, cd) = compD ds
	  val cs = compS f s
	  val cr = compR f (List.length ds) r
	in
	  cd @ cs @ cr
	end

fun compile prog = Vector.fromList (compile' prog)


end
