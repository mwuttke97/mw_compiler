(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



structure Parser (*:> PARSER*) = struct

exception Error of string

datatype token =
    IF
  | THEN
  | ELSE
  | SEMICOLON
  | LPAR
  | RPAR
  | ASIGOP
  | WHILE
  | DO
  | END
  | ADD
  | SUB
  | MUL
  | LEQ
  | VAR
  | NOP
  | RETURN
  | CON of int
  | ID  of string

fun lex nil = nil
  | lex (#" " :: cs)                                         = lex cs
  | lex (#"\n" :: cs)                                        = lex cs
  | lex (#"\t" :: cs)                                        = lex cs
  | lex (#"i" :: #"f" :: cs)                                 = IF        :: lex cs
  | lex (#"t" :: #"h" :: #"e" :: #"n" :: cs)                 = THEN      :: lex cs
  | lex (#"e" :: #"l" :: #"s" :: #"e" :: cs)                 = ELSE      :: lex cs
  | lex (#";" :: cs)                                         = SEMICOLON :: lex cs
  | lex (#"(" :: cs)                                         = LPAR      :: lex cs
  | lex (#")" :: cs)                                         = RPAR      :: lex cs
  | lex (#":" :: #"=" :: cs)                                 = ASIGOP    :: lex cs
  | lex (#"w" :: #"h" :: #"i" :: #"l" :: #"e" :: cs)         = WHILE     :: lex cs
  | lex (#"d" :: #"o" :: cs)                                 = DO        :: lex cs
  | lex (#"e" :: #"n" :: #"d" :: cs)                         = END       :: lex cs
  | lex (#"+" :: cs)                                         = ADD       :: lex cs
  | lex (#"-" :: cs)                                         = SUB       :: lex cs
  | lex (#"*" :: cs)                                         = MUL       :: lex cs
  | lex (#"<" :: #"=" :: cs)                                 = LEQ       :: lex cs
  | lex (#"v" :: #"a" :: #"r" :: cs)                         = VAR       :: lex cs
  | lex (#"n" :: #"o" :: #"p" :: cs)                         = NOP       :: lex cs
  | lex (#"r" :: #"e" :: #"t" :: #"u" :: #"r" :: #"n" :: cs) = RETURN    :: lex cs
  | lex (c :: cs) = if Char.isDigit c      then lexDigit (c::cs) 0
                    else if Char.isAlpha c then lexVar   (c::cs) []
		    else raise Error "lex"
and lexDigit cs a = if null cs orelse not (Char.isDigit (hd cs)) then (CON a) :: (lex cs)
                    else lexDigit (tl cs) (a * 10 + ord (hd cs) - ord #"0")
and lexVar   cs a = if null cs orelse not (Char.isAlpha (hd cs)) then ID (implode (rev a)) :: (lex cs)
                    else lexVar (tl cs) (hd cs :: a)

(* Parser *)

fun match (a,ts) t = if null ts orelse hd ts <> t
                     then raise Error "match"
		     else (a, tl ts)

fun extend (a,ts) p f = let val (a',tr) = p ts in (f(a,a'), tr) end


(* List of Declarations *)
exception NoDeclaration

fun parseDs ts = (case parseD ts of (d, ts') => extend (d, ts') parseDs op::) handle NoDeclaration => (nil, ts)

(* Single Declaration *)
and parseD (VAR :: (ID x) :: ASIGOP :: cs) =
	(case parseE cs of
	  (e, cs') => ((x, e), cs'))
  | parseD _ = raise NoDeclaration


(* Expressions *)
and parseE  ts = parseE' (parseE_M ts)
and parseE' (e, ADD::tr) = parseE' (extend (e,tr) parseE_M W.Add)
  | parseE' (e, SUB::tr) = parseE' (extend (e,tr) parseE_M W.Sub)
  | parseE' s = s

and parseE_M  ts = parseE_M' (parseE_L ts)
and parseE_M' (e, MUL::tr) = parseE_M' (extend (e,tr) parseE_L W.Mul)
  | parseE_M' s = s

and parseE_L  ts = parseE_L' (parseE_prim ts)
and parseE_L' (e, LEQ::tr) = parseE_L' (extend (e,tr) parseE_prim W.Leq)
  | parseE_L' s = s

and parseE_prim (CON z :: tr) = (W.Con z, tr)
  | parseE_prim (ID  z :: tr) = (W.Var  z, tr)
  | parseE_prim (LPAR  :: tr) = match (parseE tr) RPAR
  | parseE_prim _ = raise Error "parseE_prim"


(* Statements *)
exception NoStatement
fun parseS  ts = (case parseS' ts of
		  (s, SEMICOLON :: ts') => ((extend (s, ts') parseS W.SCon) handle NotStatement => raise Error "not a statement after SEMICOLON")
		| (s, ts')              => (s, ts')) handle NoStatement => (W.Nop, ts)

(* Single Statement *)
and parseS' (ID x :: ASIGOP :: ts) = extend (x, ts) parseE W.Asig
  | parseS' (IF :: ts) = 
	(case parseE ts of
	   (exp, THEN :: ts') =>
	   	(case parseS ts' of
		   (s1, ELSE :: ts'') =>
		   	let
			  val (s2, ts''') = parseS ts''
			in
			  (W.If (exp, s1, s2), ts''')
			end
		   | _ => raise Error "missing ELSE")
	   | _ => raise Error "missing THEN")

  | parseS' (WHILE :: ts) =
  	(case parseE  ts of
	   (exp, DO :: ts') =>
	   	(case parseS ts' of
		   (s, END :: ts'') => (W.While (exp, s), ts'')
		   | _ => raise Error "missing END")
	   | _ => raise Error "missing DO")

  | parseS' (LPAR :: ts) = match (parseS ts) RPAR
  | parseS' (NOP  :: ts) = (W.Nop, ts)
  | parseS' _ = raise NoStatement


(* Return Statement *)
fun parseR (RETURN :: ts) = parseE ts
  | parseR _ = raise Error "missing RETURN"

fun parse s =
	let
	  val lexed = lex (explode s)
	  val (declarations, ts1) = parseDs lexed
	  val (statement, ts2)    = parseS ts1
	  val (return, ts3)       = parseR ts2
	in
	  case ts3 of
	     [] => (declarations, statement, return)
	   |  _ => raise Error "Bulshit at the end"
	end
end
