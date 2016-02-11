(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



signature MACHINE = sig
	datatype cmd =
		  ADD
		| SUB
		| MUL
		| LEQ
		| CON  of int	(* Push an element to the stack *)
		| PUT   of int	(* Pop an element from the stack and push it to the nth position *)
		| GET   of int	(* Put the nth element from the stack to the stack *)
		| BRAN  of int	(* Add n to the PC *)
		| CBRAN of int	(* Conditional branch *)
		| HALT   	(* Halt the execution *)

	type code  = cmd vector
	type code' = cmd list

	exception Halten
	exception Error of string

	val init     : code -> unit
	
	val getStack : unit -> int list
	val getPC    : unit -> int
	val isHalten : unit -> bool

	val execute  : unit -> unit

	val run      : code  -> int list
	val run'     : code' -> int list
end
