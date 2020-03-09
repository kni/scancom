structure  ScancomPosition :
sig
  type cs
  val findShift     : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string -> ( 'a * int * int      ) option
  val findManyShift : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string -> (('a * int * int) list) option
  val findLine      : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string -> ( 'a * int * int      ) option
  val findManyLine  : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string -> (('a * int * int) list) option
end
=
struct

datatype cs = Index of int


fun findShift cvt s =
  let
    val len = String.size s

    fun rdr (Index i) =
      if i = len
      then NONE
      else SOME (String.sub(s, i), Index(i+1))

   fun doit p =
    case cvt rdr (Index p) of
        SOME (r, (Index i)) => SOME ((r, p, i))
      | NONE                => if p < len then doit (p + 1) else NONE
  in
    doit 0
  end


fun findManyShift cvt s =
  let
    val len = String.size s

    fun rdr (Index i) =
      if i = len
      then NONE
      else SOME (String.sub(s, i), Index(i+1))

   fun doit p rs =
    case cvt rdr (Index p) of
        SOME (r, (Index i)) =>
          if p < len
          then doit (p + 1) ((r, p, i)::rs)
          else SOME ((r, p, i)::rs)
      | NONE =>
          if p < len
          then doit (p + 1) rs
          else
            if List.null rs
            then NONE
            else SOME (List.rev rs)
  in
    doit 0 []
  end


fun calcCell (s, p, i, line, column) =
  if i = p then (line, column) else
  let
    val c = String.sub(s, i)
    val (line, column) = case c of #"\n" => (line + 1, 1) | #"\r" => (line, column) | _ => (line, column + 1)
  in
    calcCell (s, p, i + 1, line, column)
  end


fun findLine cvt s = case findShift cvt s of NONE => NONE | SOME (r, b, e) =>
  let val (l, c) = calcCell (s, b, 0, 1, 1) in SOME (r, l, c) end


fun findManyLine cvt s = case findManyShift cvt s of NONE => NONE | SOME rs =>
  let
    val (rs, shift) = List.foldr (
        fn ((r, b, e), (rs, shift)) => let val (l, c) = calcCell (s, b, 0, 1, 1) in ((r, l, c)::rs, shift + b) end
      ) ([], 0) rs
  in
    SOME rs
  end

end
