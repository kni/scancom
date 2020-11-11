use "../scancom.sml";
use "../scancom-replace.sml";

open ScancomReplace

fun testResult r expected name = print ((if r = expected then "" else "Not ") ^ "OK - " ^ name ^ "\n" )


val op <$> = Scancom.<$>
val scanner = (String.implode o map Char.toUpper o String.explode) <$> Scancom.takeStrI "de"


val _ = testResult (replace     scanner "abc de fgh ABC De FGH") "abc DE fgh ABC De FGH" "replace"
val _ = testResult (replaceMany scanner "abc de fgh ABC De FGH") "abc DE fgh ABC DE FGH" "replaceMany"


val _ = testResult (foldWS     "") "" "foldWS"
val _ = testResult (foldWSEdge "") "" "foldWSEdge"

val _ = testResult (foldWS     "   ") ""  "foldWS     0"
val _ = testResult (foldWSEdge "   ") " " "foldWSEdge 0"

val _ = testResult (foldWS     " Hello.\n Bye. ")  "Hello. Bye."  "foldWS     1"
val _ = testResult (foldWSEdge " Hello.\n Bye. ") " Hello. Bye. " "foldWSEdge 1"

val _ = testResult (foldWS     "  Hello.\n Bye.  ")  "Hello. Bye."  "foldWS     2"
val _ = testResult (foldWSEdge "  Hello.\n Bye.  ") " Hello. Bye. " "foldWSEdge 2"



fun cleanText t =
  let open Scancom in
    ScancomReplace.foldWS (ScancomReplace.replaceMany (takeStr "&nbsp;" *> pure " ") t)
  end

val _ = testResult (cleanText " a &nbsp; b ") "a b" "cleanText"


val _ = testResult (chomp "foo\n") "foo" "chomp"
