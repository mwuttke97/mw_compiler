(* Copyright (c) 2016 Maximilian Wuttke

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)



structure MW_Io :> MW_IO = struct

fun m_cmd Machine.ADD       = "ADD"
  | m_cmd Machine.SUB       = "SUB"
  | m_cmd Machine.MUL       = "MUL"
  | m_cmd Machine.LEQ       = "LEQ"
  | m_cmd Machine.GEQ       = "GEQ"
  | m_cmd Machine.LT        = "LT"
  | m_cmd Machine.GT        = "GT"
  | m_cmd Machine.EQ        = "EQ"
  | m_cmd Machine.HALT      = "HALT"
  | m_cmd (Machine.CON   x) = "CON\t"   ^ (Int.toString x)
  | m_cmd (Machine.PUT   x) = "PUT\t"   ^ (Int.toString x)
  | m_cmd (Machine.GET   x) = "GET\t"   ^ (Int.toString x)
  | m_cmd (Machine.BRAN  x) = "BRAN\t"  ^ (Int.toString x)
  | m_cmd (Machine.CBRAN x) = "CBRAN\t" ^ (Int.toString x)

fun print_m  x = (Vector.map (fn s => print (m_cmd s ^ "\n")) x; ())
fun print_m' x = (map (fn s => print (m_cmd s)) x; ())

end
