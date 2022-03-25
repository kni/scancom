structure  ScancomWords :
sig
  type cs
  val findManyPos : ((string, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string vector -> (('a * int * int) list) option

  type ('a, 'cs) Scanner = (string, 'cs) StringCvt.reader -> ('a, 'cs) StringCvt.reader

  val phrase      : string list -> (string list, 'cs) Scanner
  val mixture     : string list -> (string list, 'cs) Scanner
  val mixtureSkip : string list -> (string list, 'cs) Scanner
end
=
struct

datatype cs = Index of int

type ('a, 'cs) Scanner = (string, 'cs) StringCvt.reader -> ('a, 'cs) StringCvt.reader


fun findManyPos cvt s =
  let
    val len = Vector.length s

    fun rdr (Index i) =
      if i = len
      then NONE
      else SOME (Vector.sub(s, i), Index(i+1))

   fun doit p rs =
    case cvt rdr (Index p) of
        SOME (r, (Index i)) =>
          if i <= len
          then doit i ((r, p, i - p)::rs)
          else SOME ((r, p, i - p)::rs)
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


local
  fun phrase' []     r getc strm = SOME (List.rev r, strm)
    | phrase' (h::t) r getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               if c = h
               then phrase' t (c::r) getc strm
               else NONE

in
  fun phrase s = phrase' s []
end


local
  (* Remove x from a list and return flag what x was remove. *)
  fun removeFromList (x, ys) =
    let
    fun doit (_, [],         zs, r) = if r then SOME (List.rev zs) else NONE
      | doit (x, l as y::ys, zs, r) = if not r andalso x = y then doit (x, ys, zs, true) else doit (x, ys, y::zs, r)
    in
      doit (x, ys, [], false)
    end

in
  fun mixture' [] r getc strm = SOME (List.rev r, strm)
    | mixture' s  r getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               case removeFromList (c, s) of
                   NONE   => NONE
                 | SOME t => mixture' t (c::r) getc strm

  fun mixture s = mixture' s []


  fun mixtureSkip' [] skip r getc strm = SOME (List.rev r, strm)
    | mixtureSkip' s  skip r getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               case removeFromList (c, s) of
                   NONE   => if skip then mixtureSkip' s false r getc strm else NONE
                 | SOME t => mixtureSkip' t true (c::r) getc strm

  fun mixtureSkip s = mixtureSkip' s false []

end

end
