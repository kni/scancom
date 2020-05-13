use "../scancom-solid.sml";

open ScancomWordsInSolidText


fun compareResult (SOME r1) (SOME r2) = r1 = r2
  | compareResult _         _         = false


fun testResult r expected name =
  if compareResult r expected
  then print ("OK - " ^ name ^ "\n" )
  else print ("Not OK - " ^ name ^ "\n" )


val text = String.concat ["aaa", "bbb", "ccc", "ddd", "eee", "fff", "hhh", "ddd", "ccc", "zzz"]
val scanner = mixture ["ccc", "ddd"]

val r = SOME [(["ccc", "ddd"], 6, 12), (["ddd", "ccc"], 21, 27)]
val _ = testResult (findManyShift scanner text) r "mixture"
