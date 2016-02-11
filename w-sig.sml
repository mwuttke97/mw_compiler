(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



structure W = struct
	type var = string
	
	datatype exp =
	   Add of exp*exp
	 | Sub of exp*exp
	 | Mul of exp*exp
	 | Leq of exp*exp
	 | Var of var
	 | Con of int
	
	datatype stmt =
	   Asig  of var*exp
	 | If    of exp*stmt*stmt
	 | While of exp*stmt
	 | SCon  of stmt*stmt
	 | Nop
	
	type declaration = var * exp
	
	type prog = declaration list * stmt * exp
end
