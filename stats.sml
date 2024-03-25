
signature STATISTICS =
sig

    type documents = (string Seq.seq) Seq.seq

    (* given a collection of documents, compute a dictionary 
       mapping each word to the number of times it occured *)
    val frequencies : documents -> (string, int) Dict.dict

    (* given a collection of documents, compute the total number
       of distinct words in the documents *)
    val num_distinct_words : documents -> int

    (* given a collection of documents, compute a sequence (without duplicates)
       of all of the words in the documents *)
    val distinct_words : documents -> string Seq.seq

    (* given a collection of documents, compute the total number of words 
       (counting duplicates more than once) in the documents *)
    val num_words : documents -> int

end



structure Stats :> STATISTICS = 
struct
   type documents = (string Seq.seq) Seq.seq

  fun frequencies (doclist: documents) : (string, int) Dict.dict =
      let
         val doc_seq = Seq.flatten(doclist)
      in Seq.mapreduce (fn word => Dict.insert(String.compare, Dict.empty,(word,1)), Dict.empty,
         fn(dict1,dict2) => Dict.merge(String.compare, Int.+, dict1, dict2), doc_seq) end


  fun distinct_words (doclist) : string Seq.seq =
      let val doc_dict = frequencies(doclist)
          val distinct = Seq.map(fn(k,v) => k, Dict.toSeq(doc_dict))
      in distinct end

  fun num_distinct_words (doclist) : int =
      let val doc_dist = distinct_words(doclist)
            val list_length = Seq.length (doc_dist)
          in list_length end

  fun num_words (doclist) : int =
      let val word_list = Seq.flatten(doclist)
          val list_length = Seq.length (word_list)
      in list_length end

end




structure TestStats =
struct

    fun split (s : string) : string Seq.seq = Seq.fromlist (String.tokens Char.isSpace s)

    val example : Stats.documents = Seq.fromlist [ split "this is document one", split "this is document two" ]

    fun dictToList (d : (string, int) Dict.dict) : (string * int) list = Seq.tolist (Dict.toSeq d)

    (* uncomment to test *)

    open Testing
        
    fun test() =
        (testi "ndw" (Stats.num_distinct_words example) 5;
         testi "nw" (Stats.num_words example) 8;
         testsl "dw" (Seq.tolist (Stats.distinct_words example)) ["document","is","one","this","two"];
         testsil "fr" (dictToList (Stats.frequencies example)) [("document",2),("is",2),("one",1),("this",2),("two",1)]) 

end 
