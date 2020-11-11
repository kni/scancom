structure ScancomReplace :
sig
  val replace     : (string, StringCvt.cs) Scancom.Scanner -> string -> string
  val replaceMany : (string, StringCvt.cs) Scancom.Scanner -> string -> string

  val foldWS     : string -> string
  val foldWSEdge : string -> string

  val chomp : string -> string
end
=
struct

open Scancom

fun replace p s =
  let
    val scanner = search p >>= (fn (b, r) => (takeTail >>= (fn t => pure ( b ^ r ^ t ))))
  in
    case StringCvt.scanString scanner s of NONE => s | SOME s => s
  end


fun replaceMany p s =
  let
    val scanner = many (search p >>= (fn (b, r) => pure ( b ^ r ))) >>=
      (fn r => (takeTail >>= (fn t => pure (String.concat r ^ t))))
  in
    case StringCvt.scanString scanner s of NONE => s | SOME s => s
  end


fun foldWS' keepEdge s =
 let
    fun scaner prev r getc strm =
      case getc strm of
           NONE => SOME (String.implode (List.rev (if prev andalso not keepEdge andalso not (List.null r) then List.tl r else r)), strm)
         | SOME (c, strm) =>
            if Char.isSpace c
            then
              if prev
              then scaner true r getc strm
              else scaner true (#" "::r) getc strm
            else scaner false (c::r) getc strm
  in
   valOf (StringCvt.scanString (scaner (not keepEdge) []) s)
  end

val foldWS      = foldWS' false
val foldWSEdge  = foldWS' true


fun chomp s =
  let
    val size = String.size s
  in
    if size > 0 andalso String.sub (s, size - 1) = #"\n"
    then String.substring (s, 0, size - 1)
    else s
  end

end
