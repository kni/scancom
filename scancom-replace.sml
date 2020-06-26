structure ScancomReplace :
sig
  val replace     : (string, StringCvt.cs) Scancom.Scanner -> string -> string
  val replaceMany : (string, StringCvt.cs) Scancom.Scanner -> string -> string

  val chomp : string -> string
end
=
struct

open Scancom

fun replace p s =
  let
    val scanner = search p >>= (fn (b, r) => (takeTail >>= (fn t => pure ( b ^ r ^ t ))))
  in
    Option.valOf (StringCvt.scanString scanner s)
  end


fun replaceMany p s =
  let
    val scanner = many (search p >>= (fn (b, r) => pure ( b ^ r ))) >>=
      (fn r => (takeTail >>= (fn t => pure (String.concat r ^ t))))
  in
    Option.valOf (StringCvt.scanString scanner s)
  end


fun chomp s =
  let
    val size = String.size s
  in
    if size > 0 andalso String.sub (s, size - 1) = #"\n"
    then String.substring (s, 0, size - 1)
    else s
  end

end
