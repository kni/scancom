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
    fun loop (i, j, n, nss) getc strm =
      case getc strm of
           NONE => (Substring.substring (s, i, j - i - n))::(if keepEdge andalso n > 0 andalso i > 0 then [Substring.full ""] else [])
         | SOME (c, strm) =>
            if Char.isSpace c
            then loop (i, j + 1, n + 1, c <> #" ") getc strm
            else
              if n > 1 andalso j > 0 orelse nss
              then
                if i = 0 andalso not keepEdge
                then (loop (j, j + 1, 0, false) getc strm)
                else (Substring.substring (s, i, j - i - n))::
                     (loop (j, j + 1, 0, false) getc strm)
              else    loop (i, j + 1, 0, false) getc strm

    fun scaner getc strm = SOME (loop (0, 0, 0, false) getc strm, strm)
  in
    Substring.concatWith " " (Option.valOf (StringCvt.scanString scaner s))
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
