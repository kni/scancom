open Scancom

datatype Index = Index of int

val scanString = StringCvt.scanString


fun scanSubstring cvt s =
  let
    val len = Substring.size s

    fun rdr (Index i) =
      if i = len
      then NONE
      else SOME (Substring.sub(s, i), Index(i+1))
  in
    case cvt rdr (Index 0) of
         SOME (r, (Index i)) => SOME (r, Substring.triml i s )
       | NONE                => NONE
  end


(*
local
  fun showResult r = case r of SOME s => print ("SOME " ^ s ^ "\n") | NONE => print ("NONE\n")
in

  val r = scanString (takeStr "--") "--abcde-=*xyz"
  val _ = showResult r

  val r = scanString (takeBefore "-=*") "abcde-=*xyz"
  val _ = showResult r

  val r = scanString (takeStr "-=*") "-=*xyz"
  val _ = showResult r

  val r = scanString takeTail "xyz"
  val _ = showResult r

  val _ = print "----\n\n"

end

val parseArg = (takeStr "--") *> (takeBefore "-=*") >>= (fn k => (takeStr "-=*") *> takeTail >>= (fn v => pure (k, v)))
val r = scanString parseArg "--abcde-=*xyz"
val _ = case r of SOME (k, v) => print ("SOME '" ^ k ^ "' is '" ^ v ^ "'\n") | NONE => print ("NONE\n")
*)


(* http://stackoverflow.com/questions/14750444/how-can-i-parse-string-to-int-int-tuple-in-sml *)
val scanLine = takeInt >>= (fn x => takeStr "," *> takeInt >>= (fn y => takeStr "\n" *> pure (x, y) ) )
val scanList = many scanLine

(*
fun showListPair []         = ()
  | showListPair ((x,y)::t) = ( print ((Int.toString x) ^ " " ^ (Int.toString y) ^ "\n") ; showListPair t )

val r = scanString scanList "4,5\n2,3\n-"
val _ = case r of NONE => print ("NONE\n") | SOME l => ( print ("SOME\n") ; showListPair l )
*)



val scanRedis = takeStr "$" *> takeInt >>= (fn n => takeStr "\r\n" *> takeN n <* takeStr "\r\n")

(*
val r = scanString scanRedis "$4\r\nINFO\r\nTAIL"
val _ = case r of NONE => print ("NONE\n") | SOME cmd => print ("SOME " ^ cmd ^ "\n")
*)


fun runBench name n f s =
let
  fun loop 0 = ()
    | loop i = (f s; loop (i - 1))

  val t0 = Time.now ()
  val _ = loop n
  val t1 = Time.now ()
in
  print (name ^ " " ^ Real.toString(Time.toReal(Time.-(t1, t0))) ^ "\n")
end



val N = 10000000
val sfull = Substring.full

fun main () = (
  print "Run Benckmark...\n";
  runBench "Bench scanRedis, String   " N (scanString scanRedis)           "$4\r\nINFO\r\nTAIL";
  runBench "Bench CSV,       String   " N (scanString scanList)            "4,5\n2,3\n-"
  (*
  runBench "Bench scanRedis, Substring" N (scanSubstring scanRedis) (sfull "$4\r\nINFO\r\nTAIL");
  runBench "Bench CSV,       Substring" N (scanSubstring scanList)  (sfull "4,5\n2,3\n-")
  *)
  )
