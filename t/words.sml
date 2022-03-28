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




(*
Шукаємо найкращій збіг.
Якщо знайдено слово, яке знайдено до того і є першим серед знайдених,
то починати пошук зі зсувом на одну позицію.
Це потрібно щоб в "xxx car yyy bike car bus zzz" для ключових слів "bike car bus" знайти
"bike car bus", а не "car yyy bike car bus".
*)

val text = Vector.fromList ["xxx", "car", "yyy", "bike", "car", "bus", "zzz"]
val scanner = mixtureSkip ["bike", "car", "bus"]
val r = findManyPos scanner text
val e = SOME [(["bike", "car", "bus"], 3, 3)]
val _ = testResult r e "mixtureSkip: fold"

val scanner = mixtureSkip ["bike", "car", "car", "bus"]
val r = findManyPos scanner text
val e = SOME [(["car", "bike", "car", "bus"], 1, 5)]
val _ = testResult r e "mixtureSkip: fold duble"
