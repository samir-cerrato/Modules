(*  

   NON-COLLABORATIVE CHALLENGE PROBLEM
   25 points

   All of the previous homeworks have had copied and pasted test
   functions for each type -- many of these are below.
   The 'print' function prints strings on the screen.  

   Use higher-order functions and polymorphism to abstract this repeated
   code, avoiding as much code duplication as possible.  

   Show how to give more concise definations of the test functions below
     testi
     testii
     testil
     testisl
     testilsl
     testb
     testill
     testr
     testrr
   as instances of your abstract version.  

   The way that the your test function prints the output and expected output
   does not have to match the functions below exactly, as long 
   as the necessary information is displayed unambiguously
   (e.g. in can differ in whitespace, parentheses).  

*)

fun testi (s : string) (n : int) (m : int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

fun testii (s : string) (n : int * int) (m : int * int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => let val (x,y) = n
                     val (x',y') = m
                 in
                     print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString x' ^ " , " ^ Int.toString y'  ^ "\n    Got: " ^ Int.toString x ^ " , " ^ Int.toString y ^ "\n")
                 end

fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)
                        
fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

fun slToString(l : string list) : string =
    case l of
        [] => "[]"
      | x :: xs => x ^ "::" ^ slToString(xs)

fun islToString(l : (int * string) list) : string =
    case l of
        [] => "[]"
      | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ s ^ ")" ^ "::" ^ islToString(xs)

fun testisl (s : string) (n : (int * string) list) (m : (int * string) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ islToString m ^ "\n    Got: " ^ islToString n ^ "\n")

fun testilsl (s : string) ((is,ss) : int list * string list) ((is',ss') : int list * string list) : unit =
    case (is,ss) = (is',ss') of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString is' ^ "," ^ slToString ss' ^ "\n    Got: " ^ ilToString is ^ "," ^ slToString ss ^  "\n")

fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

fun illToString(l : int list list) : string =
    case l of
        [] => "[]"
      | x :: xs => "(" ^ ilToString x ^ ") :: " ^ illToString(xs)

fun testill (s : string) (n : (int list) list) (m : (int list) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ illToString m ^ "\n    Got: " ^ illToString n ^ "\n")

val epsilon = 0.000000000001
            
fun testr (s : string) (n : real) (m : real) : unit =
    case Real.abs(n - m) < epsilon of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Real.toString m ^ "\n    Got: " ^ Real.toString n ^ "\n")

fun testrr (s : string) (n : real * real) (m : real * real) : unit =
    let val (x,y) = n
        val (x',y') = m
    in
        case Real.abs(x - x') < epsilon andalso Real.abs(y - y') < epsilon of
            true => print ("Test " ^ s ^ " OK\n")
          | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Real.toString x' ^ " , " ^ Real.toString y'  ^ "\n    Got: " ^ Real.toString x ^ " , " ^ Real.toString y ^ "\n")
    end

(*NEW REWRITTEN CODES*)

fun rewrite(s: string, got_value : 'a, expected_val : 'a, to_string : 'a -> string, compare : ('a * 'a) -> bool) : unit =
  case compare(got_value, expected_val) of
       true => print ("Test " ^ s ^ " OK\n")
     |false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ islToString m ^ "\n    Got: " ^ islToString n ^ "\n")

fun testi (s : string) (n : int) (m : int) : unit =
    rewrite (s, fn x => Int.toString x, fn (x, y) => (x = y), n, m)

fun testii (s : string) (n : int * int) (m : int * int) : unit =
    rewrite (s, n , m, fn (x, y) => Int.toString x ^ " , " ^ Int.toString y, fn (x, y) => x * y)

fun testil (s : string) (n : int list) (m : int list) : unit =
    rewrite (s, n, m, ilToString, fn (x, y) => x * y)

fun testisl (s : string) (n : (int * string) list) (m : (int * string) list) : unit =
    rewrite (s, n, m, islToString, fn (x,y) => n * m)

fun testilsl (s : string) ((is.ss) : int list * string list) ((is', ss') : int list * string list) : unit =
    rewrite (s, (is, ss), fn (x,y) => ilToString x ^ "," ^ slToString y, fn ((a,b),(c,d)) => (a, b) = (c, d))

fun testb (s : string) (n : bool) (m : bool) : unit =
    rewrite (s, n, m, fn x => Bool.toString x, fn (x, y) => x = y)

fun testill (s : string) (n : (int list) list) (m : (int list) list) : unit =
    rewrite (s, m, n, illToString, fn (x,y) => x = y)

fun testr (s : string) (n : real) (m : real) : unit =
    rewrite (s, n, m, fn x => Real.toString x, fn (x,y) => Real.abs(n - m) < epsilon)

fun testrr (s : string) (n : real * real) (m : real * real) : unit =
    rewrite (s, n, m, fn (x, y) => Real.toString x ^ " , " ^ Real.toString y, fn ((x, y),(x', y')) => Real.abs(x - x') < epsilon andalso Real.abs(y - y') < epsilon)

