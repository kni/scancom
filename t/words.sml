use "../scancom-words.sml";

open ScancomWords

fun compareResult (SOME r1) (SOME r2) = r1 = r2
  | compareResult _         _         = false


fun testResult r expected name =
  if compareResult r expected
  then print ("OK - " ^ name ^ "\n" )
  else print ("Not OK - " ^ name ^ "\n" )


val text = Vector.fromList ["aaa", "bbb", "ccc", "ddd", "eee", "fff", "hhh", "ccc", "ddd", "zzz"]
val scanner = phrase ["ccc", "ddd"]
val r = SOME [(["ccc", "ddd"], 2, 2), (["ccc", "ddd"], 7, 2)]
val _ = testResult (findManyPos scanner text) r "phrase"


val text = Vector.fromList ["aaa", "bbb", "ccc", "ddd", "eee", "fff", "hhh", "ddd", "ccc", "zzz"]
val scanner = mixture ["ccc", "ddd"]
val r = SOME [(["ccc", "ddd"], 2, 2), (["ddd", "ccc"], 7, 2)]
val _ = testResult (findManyPos scanner text) r "mixture"


val text = Vector.fromList ["aaa", "bbb", "ccc", "xxx", "ddd", "eee", "fff", "hhh", "ddd", "ccc", "zzz"]
val scanner = mixtureSkip ["ccc", "ddd"]
val r = SOME [(["ccc", "ddd"], 2, 3), (["ddd", "ccc"], 8, 2)]
val _ = testResult (findManyPos scanner text) r "mixtureSkip"


val text = Vector.fromList ["abc", "abc", "abc", "abc", "abc", "abc"]
val scanner = mixtureSkip ["abc", "abc"]
val r = SOME [(["abc", "abc"], 0, 2), (["abc", "abc"], 2, 2), (["abc", "abc"], 4, 2)]
val _ = testResult (findManyPos scanner text) r "mixtureSkip: no overlapping"
