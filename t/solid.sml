use "../scancom-solid.sml";

open ScancomWordsInSolidText


fun compareResult (SOME r1) (SOME r2) = r1 = r2
  | compareResult _         _         = false


fun testResult r expected name =
  if compareResult r expected
  then print ("OK - " ^ name ^ "\n" )
  else print ("Not OK - " ^ name ^ "\n" )


fun printResult (text, r) = List.app (fn (_, i, j) => print ((String.substring (text, i, j)) ^ "\n")) (valOf r)


val text = String.concat ["aaa", "bbb", "ccc", "ddd", "eee", "fff", "hhh", "ddd", "ccc", "zzz"]
val scanner = mixture ["ccc", "ddd"]
val e = SOME [(["ccc", "ddd"], 6, 6), (["ddd", "ccc"], 21, 6)]
val r = findManyShift scanner text
val _ = testResult r e "mixture"
(* val _ = printResult (text, r) *)


val text = String.concat ["abc", "abc", "abc", "abc", "abc", "abc"]
val scanner = mixture ["abc", "abc"]
val e = SOME [(["abc", "abc"], 0, 6), (["abc", "abc"], 6, 6), (["abc", "abc"], 12, 6)]
val r = findManyShift scanner text
val _ = testResult r e "mixture: no overlapping"


val text = "abc"
val scanner = mixture []
val e = SOME [([], 0, 0), ([], 1, 0), ([], 2, 0), ([], 3, 0)]
val r = findManyShift scanner text
val _ = testResult r e "mixture: search empty string"



val text = "abc"
val scanner = mixture ["abc"]
val e = SOME [(["abc"], 0, 3)]
val r = findManyShift scanner text
val _ = testResult r e "mixture: one"
