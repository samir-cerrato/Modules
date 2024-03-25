
structure Testing =
struct

    fun ioToString(x : int option) =
        case x of
            NONE => "NONE"
          | SOME x => "SOME(" ^ Int.toString x ^ ")"

    (* test a function that returns an int *)
    fun testi (s : string) (n : int) (m : int) : unit =
        case n = m of
            true => print ("Test " ^ s ^ " OK\n")
          | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

    fun testio (s : string) (n : int option) (m : int option) : unit =
        case n = m of
            true => print ("Test " ^ s ^ " OK\n")
          | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ioToString m ^ "\n    Got: " ^ ioToString n ^ "\n")

    fun silToString(l : (string * int) list) : string =
        case l of
            [] => "[]"
          | (n,s) :: xs => "(" ^ n ^ "," ^ Int.toString s ^ ")" ^ "::" ^ silToString(xs)
                
    fun testsil (s : string) (n : (string * int) list) (m : (string * int) list) : unit =
        case n = m of
            true => print ("Test " ^ s ^ " OK\n")
          | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ silToString m ^ "\n    Got: " ^ silToString n ^ "\n")

    fun iilToString(l : (int * int) list) : string =
        case l of
            [] => "[]"
          | (n,s) :: xs => "(" ^ Int.toString n ^ "," ^ Int.toString s ^ ")" ^ "::" ^ iilToString(xs)
                
    fun testiil (s : string) (n : (int * int) list) (m : (int * int) list) : unit =
        case n = m of
            true => print ("Test " ^ s ^ " OK\n")
          | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ iilToString m ^ "\n    Got: " ^ iilToString n ^ "\n")

    fun slToString(l : (string) list) : string =
        case l of
            [] => "[]"
          | s :: xs => s ^ "::" ^ slToString(xs)

    fun testsl (s : string) (n : string list) (m : string list) : unit =
        case n = m of
            true => print ("Test " ^ s ^ " OK\n")
          | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ slToString m ^ "\n    Got: " ^ slToString n ^ "\n")

end
