
structure TreeDict :> DICT =
struct

  datatype ('k, 'v) tree =
      Empty
    | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type ('k,'v) dict = ('k, 'v) tree 

  val empty = Empty

  fun size t =
        case t of
            Empty => 0
          | Node(l,_,r) => 1 + size l + size r
      
  fun insert (cmp, d, (k, v)) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => Node (L, (k, v), R)
      | LESS => Node (insert (cmp, L, (k, v)), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert (cmp, R, (k, v)))

  fun lookup (cmp, d, k) =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup (cmp, L, k)
      | GREATER => lookup (cmp, R, k)

(*TASK: Create toSeq, splitAt, and merge functions*)

fun toSeq(dict) : ('k * 'v) Seq.seq =
    case dict of
      Empty => Seq.empty()
      | Node (l, x, r) => Seq.append(Seq.append(toSeq l, Seq.singleton(x)), toSeq r)

fun splitAt(dict, key, compare) =
    case dict of  
      Empty => (Empty, NONE, Empty)
      | Node (l, (k,v), r) => (case compare (key, k) of
                                EQUAL => (l, SOME v, r)
                                | GREATER => (let val (rightl, b, rightr) = splitAt(r, key, compare)
                                              in (Node (l, (k,v), rightl), b, rightr) end)
                                    | LESS => (let val (leftl, b, leftr) = splitAt(l, key, compare)
                                                in (leftl, b, Node(leftr, (k,v), r)) end)
                              )       
fun merge(compare, combine, dict1, dict2):('k,'v) dict =
    case (dict1, dict2) of
         (Empty, Empty) => Empty
          | (_, Empty) => dict1
            | (Empty, _) => dict2
              | (Node (l,(k,v),r), Node(l2,(k2,v2),r2)) => let val (a,b,c) = splitAt(dict2, k, compare)
                                                      in
                                                      case b of
                                                           NONE => Node(merge(compare, combine, l, a), (k,v), merge(compare, combine, r, c))
                                                            |SOME d => Node (merge(compare, combine, l, a),(k, combine(v,d)), merge(compare, combine, r, c))
                                                      end


end

structure Dict = TreeDict




structure TestDict =
struct

    open Testing
    
    fun test() =
        let
            fun ins(d,(k,v)) = Dict.insert(Int.compare, d, (k,v))
            fun look(d,k) = Dict.lookup(Int.compare, d, k)
            
            val d = Dict.empty
            val d2 = ins (ins (d, (1,1)), (2,2))
            val d3 = ins (ins (ins (d, (1,1)), (1,2)), (2,2))

            val d4 = ins (ins (ins (d, (1,11)), (3,13)), (5,15))
            val d6 = ins (ins (ins (d, (1,21)), (2,22)), (4,24))
                
            val dm = Dict.merge (Int.compare, fn (x,_) => x , d4,d6)
            val dm2 = Dict.merge (Int.compare, fn (_,y) => y, d4,d6)
        in
            (
             testio "l1" (look( dm  , 1)) (SOME 11);
             testio "l2" (look( dm  , 2)) (SOME 22);
             testio "l3" (look( dm  , 3)) (SOME 13);
             testio "l4" (look( dm  , 4)) (SOME 24);
             testio "l5" (look( dm  , 5)) (SOME 15);
             testio "l6" (look( dm2 , 1)) (SOME 21);
             testiil "ts1" (Seq.tolist (Dict.toSeq d3)) [(1,2),(2,2)] 
             )
        end
             

end

