(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



signature STACKREGISTER = sig
	exception OutOfMemory
	exception NotAlocated
	exception Empty

	val pop  : unit -> int		(* Empty *)
	val push : int -> unit		(* OutOfMemory *)
	val init : unit -> unit
	val sub  : int -> int		(* Subscript *)
	val put  : (int * int) -> unit	(* OutOfMemory, Subscript *)

	val getStack : unit -> int list
end

structure Stackregister (* :> STACKREGISTER *) = struct
	exception OutOfMemory
	exception NotAlocated
	exception Empty

	val size  = 1000			(* maximum size *)
	val stack = Array.array (size, ~1)	(* Stack-Array *)
	val lar   = ref ~1			(* last allocated register on the stack *)

	(* Pop an element from the stack *)
	fun pop () =  if !lar < 0 then raise Empty
                      else #1(Array.sub (stack, !lar), lar := !lar - 1)

	(* Push an element to the stack *)
	fun push x = if !lar + 1 >= size then raise OutOfMemory
	             else (lar := !lar + 1; Array.update (stack, !lar, x))

	(* Clear the stack *)
	fun init () = (Array.modify (fn _ =>  ~1) stack; lar := ~1)

	(* Get the nth element from the stack *)
	fun sub n = if n > !lar then raise NotAlocated
	            else Array.sub (stack, n)

	fun put (n, x) = if n > !lar then raise NotAlocated
	                 else Array.update (stack, n, x)
	
	fun getStack () = List.tabulate (!lar+1, fn x => Array.sub (stack, x))
end


structure Machine :> MACHINE = struct
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

	val code   = ref (Vector.fromList [HALT])
	val halten = ref false
	val pc     = ref 0

	fun init c = (code := c; Stackregister.init (); halten := false; pc := 0)

	fun getStack () = Stackregister.getStack ()
	fun getPC    () = !pc
	fun isHalten () = !halten

	(* Executes the cmd and returns the jump *)
	fun cmd (HALT)    = (halten := true; 1)
	  | cmd (ADD)     = (let val (a, b) = (Stackregister.pop(), Stackregister.pop()) in Stackregister.push (a+b) end; 1)
	  | cmd (MUL)     = (let val (a, b) = (Stackregister.pop(), Stackregister.pop()) in Stackregister.push (a*b) end; 1)
	  | cmd (SUB)     = (let val (a, b) = (Stackregister.pop(), Stackregister.pop()) in Stackregister.push (a-b) end; 1)
	  | cmd (LEQ)     = (let val (a, b) = (Stackregister.pop(), Stackregister.pop()) in Stackregister.push (if a <= b then 1 else 0) end; 1)
	  | cmd (CON  x)  = (Stackregister.push x; 1)
	  | cmd (PUT   n) = (let val x = Stackregister.pop() in Stackregister.put (n, x) end; 1)
	  | cmd (GET   n) = (let val y = Stackregister.sub n in Stackregister.push y end; 1)
	  | cmd (BRAN  n) = n
	  | cmd (CBRAN n) = if Stackregister.pop () = 0 then n else 1

	fun execute () = if isHalten () then raise Halten
	                 else let val jump = cmd (Vector.sub (!code, !pc))
			      in (pc := !pc + jump)
			      end
	
	fun run code = (init code; while not (isHalten ()) do (execute ()); getStack ())
	fun run' code = run (Vector.fromList code)

end

