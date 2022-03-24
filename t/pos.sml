use "../scancom.sml";
use "../scancom-position.sml";

open ScancomPosition


fun compareResult (SOME r1) (SOME r2) = r1 = r2
  | compareResult _         _         = false


fun testResult r expected name =
  if compareResult r expected
  then print ("OK - " ^ name ^ "\n" )
  else print ("Not OK - " ^ name ^ "\n" )


val scanString = StringCvt.scanString
val _ = testResult (scanString (Scancom.find (Scancom.takeStr "XYZ")) "abcdeXYZfhg") (SOME "XYZ") "find takeStr"
val _ = testResult (scanString (Scancom.many (Scancom.find (Scancom.takeStr "XYZ"))) "abcdeXYZfhg") (SOME ["XYZ"]) "many find takeStr"


val op *> = Scancom.*>
val _ = testResult (scanString (Scancom.many (Scancom.find (Scancom.takeStr "x" *> Scancom.takeInt))) "---x11x22") (SOME [11, 22]) "many find takeStr"
val _ = print "\n"



val str     = "abcdexyzfhg\n" ^ "abcdeXYZfhg\n"
val scanner = Scancom.takeStrI "xyz"


val _ = testResult (findShift scanner str) (SOME ("xyz", 5, 3)) "findShift"

val _ = testResult (findManyShift scanner str) (SOME [("xyz", 5, 3), ("XYZ", 17, 3)]) "findManyShift"


val _ = testResult (findManyShift (Scancom.takeStr "x" *> Scancom.takeInt) "---x11x22") (SOME [(11, 3, 3), (22, 6, 3)]) "findManyShift int"

val _ = testResult (findManyShift (Scancom.takeStr "xyz") "xyzxyz") (SOME [("xyz", 0, 3), ("xyz", 3, 3)]) "findManyShift xyz"
val _ = testResult (findManyShift (Scancom.takeStr "x") "xx") (SOME [("x", 0, 1), ("x", 1, 1)]) "findManyShift xx"
val _ = testResult (findManyShift (Scancom.takeStr "x") "x") (SOME [("x", 0, 1)]) "findManyShift x"

val _ = print "\n"


val _ = testResult (findLine scanner str) (SOME ("xyz", 1, 6)) "findLine"

val _ = testResult (findManyLine scanner str) (SOME [("xyz", 1, 6), ("XYZ", 2, 6)]) "findManyLine"
