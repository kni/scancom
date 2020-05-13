open Scancom

val scanString = StringCvt.scanString

fun showResult r = case r of SOME s => print ("SOME " ^ s ^ "\n") | NONE => print ("NONE\n")


fun compareResult (SOME r1) (SOME r2) = r1 = r2
  | compareResult _         _         = false


fun testResult r expected name =
  if compareResult r expected
  then print ("OK - " ^ name ^ "\n" )
  else print ("Not OK - " ^ name ^ "\n" )



fun parseArgTestParts () = (
  testResult (scanString (takeStr "--") "--abcde-=*xyz")   (SOME "--")    "takeStr";
  testResult (scanString (takeBefore "-=*") "abcde-=*xyz") (SOME ("abcde", "-=*")) "takeBefore";
  testResult (scanString (takeStr "-=*") "-=*xyz")         (SOME "-=*")   "takeStr 2";
  testResult (scanString (takeTail) "xyz")                 (SOME "xyz")   "takeTail"
)


fun parseArgTest () =
  let
    val parseArg = (takeStr "--") *> (takeBefore "-=*") >>= (fn (k, _) => takeTail >>= (fn v => pure (k, v)))
  in
    testResult (scanString (parseArg) "--abcde-=*xyz") (SOME ("abcde", "xyz")) "parseArg"
  end



(* http://stackoverflow.com/questions/14750444/how-can-i-parse-string-to-int-int-tuple-in-sml *)
val scanLine = takeInt >>= (fn x => takeStr "," *> takeInt >>= (fn y => takeStr "\n" *> pure (x, y) ) )
val scanList = many scanLine

fun testCSV () =
  let
    fun showListPair []         = ()
      | showListPair ((x,y)::t) = ( print ((Int.toString x) ^ " " ^ (Int.toString y) ^ "\n") ; showListPair t )

    val r = scanString scanList "4,5\n2,3\n-"
  in
    testResult r (SOME [(4, 5), (2, 3)]) "testCSV"
    (* ; case r of NONE => print ("NONE\n") | SOME l => ( print ("SOME\n") ; showListPair l ) *)
  end



val scanRedis = takeStr "$" *> takeInt >>= (fn n => takeStr "\r\n" *> takeN n <* takeStr "\r\n")

fun testRedis () = testResult (scanString scanRedis "$4\r\nINFO\r\nTAIL") (SOME "INFO") "testRedis"


fun testTillAndWhile () = (
    testResult ( scanString (takeTill Char.isSpace) "   a" )  (SOME "")    "takeTill ws";     (* SOME ""    *)
    testResult ( scanString (takeTill Char.isSpace) "a" )     (SOME "a")   "takeTill";        (* SOME "a"   *)
    testResult ( scanString (takeWhile Char.isSpace) "   a" ) (SOME "   ") "takeWhile ws";    (* SOME "   " *)
    testResult ( scanString (takeWhile Char.isSpace) "a" )    (SOME "")    "takeWhile";       (* SOME ""    *)

    testResult ( scanString (takeTill  (fn c => c =  #"/") *> takeTail) "abc/def" ) (SOME "/def") "takeTail  and takeTail";
    testResult ( scanString (takeWhile (fn c => c <> #"/") *> takeTail) "abc/def" ) (SOME "/def") "takeWhile and takeTail"
  )



local
  val takeWS = takeWhile Char.isSpace

  val scanner =
    takeStr "charset" *>
    takeWS *>
    takeStr "=" *>
    takeWS *>
    takeTill (fn c => Char.isSpace c orelse c = #";")

  val scanner = find scanner
in
  fun findCharset s =
    case StringCvt.scanString scanner (String.map Char.toLower s) of
         NONE => NONE
       | SOME c => SOME (String.map Char.toUpper c)
end

fun testFindCharset () = (
    testResult ( findCharset "text/html; charset=Utf-8" )      (SOME "UTF-8") "findCharset 1";
    testResult ( findCharset "text/html; CharSet = Utf-8 ; " ) (SOME "UTF-8") "findCharset 2"
)



fun sample () = (
    testResult ( scanString (takeStr "abc")    "abcDe") (SOME "abc") "takeStr";
    testResult ( scanString (takeStrI "abC")   "aBcDe") (SOME "aBc") "takeStrI";
    testResult ( scanString (takeBefore "De")  "abcDe") (SOME ("abc", "De")) "takeBefore";
    testResult ( scanString (takeBeforeI "dE") "aBcDe") (SOME ("aBc", "De")) "takeBeforeI";
    parseArgTestParts ();
    parseArgTest ();
    testCSV ();
    testRedis ();
    testTillAndWhile();
    testFindCharset()
)



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

fun benckmark () = (
  print "Run Benckmark...\n";
  runBench "Bench scanRedis, String   " N (scanString scanRedis)           "$4\r\nINFO\r\nTAIL";
  runBench "Bench CSV,       String   " N (scanString scanList)            "4,5\n2,3\n-"
  )


fun main () = (
    sample ()
    (* ; benckmark () *)
  )
