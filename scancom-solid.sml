structure ScancomWordsInSolidText :
sig
  type cs
  val findManyShift : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) -> string -> (('a * int * int) list) option

  type ('a, 'cs) Scanner = (char, 'cs) StringCvt.reader -> ('a, 'cs) StringCvt.reader

  val mixture : string list -> (string list, 'cs) Scanner
end
=
struct

datatype cs = Index of int

type ('a, 'cs) Scanner = (char, 'cs) StringCvt.reader -> ('a, 'cs) StringCvt.reader


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
          then doit (p + 1) ((r, p, i - p)::rs)
          else SOME ((r, p, i - p)::rs)
      | NONE                =>
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
  fun compare s []     getc strm = SOME (s, strm)
    | compare s (h::t) getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               if c = h
               then compare s t getc strm
               else NONE

  fun takeStr s = compare s (String.explode s)


  (* Remove x from a list and return flag what x was remove. *)
  fun removeFromList ys getc strm =
    let
    fun doit ([], zs, r)    getc strm = ( case r of SOME y => SOME (y, List.rev zs, strm) | NONE => NONE )
      | doit (y::ys, zs, r) getc strm = ( case r of SOME _ => doit (ys, y::zs, r) getc strm | NONE =>
                                               case takeStr y getc strm of
                                                   SOME (s, strm) => doit (ys, zs, SOME y) getc strm
                                                 | NONE           => doit (ys, y::zs, r) getc strm
                                             )
    in
      doit (ys, [], NONE) getc strm
    end


  fun mixture' [] r getc strm = SOME (List.rev r, strm)
    | mixture' s  r getc strm =
               case removeFromList s getc strm of
                   NONE              => NONE
                 | SOME (c, t, strm) => mixture' t (c::r) getc strm
in
  fun mixture s = mixture' s []
end

end
