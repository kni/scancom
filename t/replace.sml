use "../scancom.sml";
use "../scancom-replace.sml";

open ScancomReplace

fun testResult r expected name = print ((if r = expected then "" else "Not ") ^ "OK - " ^ name ^ "\n" )


val op <$> = Scancom.<$>
val scanner = (String.implode o map Char.toUpper o String.explode) <$> Scancom.takeStrI "de"


val _ = testResult (replace     scanner "abc de fgh ABC De FGH") "abc DE fgh ABC De FGH" "replace"
val _ = testResult (replaceMany scanner "abc de fgh ABC De FGH") "abc DE fgh ABC DE FGH" "replaceMany"


val _ = testResult (chomp "foo\n") "foo" "chomp"
