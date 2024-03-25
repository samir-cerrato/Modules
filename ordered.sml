signature ORDERED = 
sig
    type t
    val compare : t * t -> order 
end

structure StringLt : ORDERED = 
struct
    type t = string
    val compare = String.compare 
end


structure IntLt : ORDERED = 
struct
    type t = int
    val compare = Int.compare 
end

