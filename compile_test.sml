(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



CM.make "sources.cm";
open W;
open Compiler;
open Machine;
open Parser;
open MW_Io;

val testprog : prog = ([("x", Con 3)], Nop, Add (Var "x", Con 1));
val compiled : code = compile testprog


val prog1 = "var a := 5 nop return a"
val prog2 = "var n := 10 var a := 1 while 2<=n do a := n*a; n := n-1 end return a"
val prog3 = "var a := 2 var b := 3 var y := 1 while 1<=b do y := y*a; b := b-1 end return y"
(*
 0	CON	3
 1	CON	1
 2	GET	0
 3	CON	2
 4	LEQ
 5	CBRAN	10
 6	GET	1
 7	GET	0
 8	MUL
 9	PUT	1
10	CON	1
11	GET	0
12	SUB
13	PUT	0
14	BRAN	~12
15	GET	1
16	PUT	1
17	PUT	0
18	HALT
*)
