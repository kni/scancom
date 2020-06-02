infix 4 <$>
infix 1 >>=
infix 4 *>
infix 4 <*
infix 4 <*>

signature SCANCOM = sig
  type ('a, 'cs) Scanner = (char, 'cs) StringCvt.reader -> ('a, 'cs) StringCvt.reader

  val takeStr     : string -> (string, 'cs) Scanner
  val takeStrI    : string -> (string, 'cs) Scanner
  val takeBefore  : string -> (string * string, 'cs) Scanner
  val takeBeforeI : string -> (string * string, 'cs) Scanner
  val takeInt     : (int, 'cs) Scanner
  val takeN       : int -> (string, 'cs) Scanner
  val takeTill    : (char -> bool) -> (string, 'cs) Scanner
  val takeWhile   : (char -> bool) -> (string, 'cs) Scanner
  val takeTail    : (string, 'cs) Scanner

  val pure   : 'a -> ('a, 'cs) Scanner
  val fail   : ('a, 'cs) Scanner
  val fmap   : ('a -> 'b) -> ('a, 'cs) Scanner -> ('b, 'cs) Scanner
  val bind   : ('a, 'cs) Scanner -> ('a -> ('b, 'cs) Scanner) -> ('b, 'cs) Scanner
  val apR    : ('a, 'cs) Scanner -> ('b, 'cs) Scanner -> ('b, 'cs) Scanner
  val apL    : ('a, 'cs) Scanner -> ('b, 'cs) Scanner -> ('a, 'cs) Scanner
  val ap     : (('a -> 'b), 'cs) Scanner -> ('a, 'cs) Scanner -> ('b, 'cs) Scanner
  val choice : ('a, 'cs) Scanner list -> ('a, 'cs) Scanner
  val many   : ('a, 'cs) Scanner -> ('a list, 'cs) Scanner
  val find   : ('a, 'cs) Scanner -> ('a, 'cs) Scanner
  val search : ('a, 'cs) Scanner -> ((string * 'a), 'cs) Scanner

  val <$> : ('a -> 'b) * ('a, 'cs) Scanner -> ('b, 'cs) Scanner
  val >>= : ('a, 'cs) Scanner * ('a -> ('b, 'cs) Scanner) -> ('b, 'cs) Scanner
  val  *> : ('a, 'cs) Scanner * ('b, 'cs) Scanner -> ('b, 'cs) Scanner
  val <*  : ('a, 'cs) Scanner * ('b, 'cs) Scanner -> ('a, 'cs) Scanner
  val <*> : (('a -> 'b), 'cs) Scanner * ('a, 'cs) Scanner -> ('b, 'cs) Scanner
end

structure Scancom : SCANCOM = struct

  type ('a, 'cs) Scanner = (char, 'cs) StringCvt.reader -> ('a, 'cs) StringCvt.reader

  local

  fun takeChar' s tc getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) => if c = tc then SOME (s, strm) else NONE

  fun takeCharI' tc getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               if c = tc orelse Char.toLower c = tc
               then SOME (String.str c, strm)
               else NONE


  (* compare and return if match *)
  fun takeStr' s []     getc strm = SOME (s, strm)
    | takeStr' s (h::t) getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               if c = h
               then takeStr' s t getc strm
               else NONE

  (* insensitive compare and return if match *)
  fun takeStrI' r []     getc strm = SOME (String.implode (List.rev r), strm)
    | takeStrI' r (h::t) getc strm =
        case getc strm of
             NONE           => NONE
           | SOME (c, strm) =>
               if c = h orelse Char.toLower c = h
               then takeStrI' (c::r) t getc strm
               else NONE
  in

  fun takeStr s =
    if String.size s = 1
    then takeChar' s (String.sub (s, 0))
    else takeStr'  s (String.explode s)


  (* insensitive *)
  fun takeStrI s =
    if String.size s = 1
    then takeCharI' (String.sub (s, 0))
    else takeStrI' [] (map Char.toLower (String.explode s))



  fun takeBefore s =
    let
      val e = String.explode s

      fun scan r getc strm =
        case takeStr' s e getc strm of
             SOME (s, strm) => SOME ((String.implode (List.rev r), s), strm)
           | NONE        =>
                case getc strm of
                    NONE => NONE
                  | SOME (c, strm) => scan (c::r) getc strm

    in
      scan []
    end


  fun takeBeforeI s =
    let
      val e = map Char.toLower (String.explode s)

      fun scan r getc strm =
        case takeStrI' [] e getc strm of
             SOME (s, strm) => SOME ((String.implode (List.rev r), s), strm)
           | NONE        =>
                case getc strm of
                    NONE => NONE
                  | SOME (c, strm) => scan (c::r) getc strm

    in
      scan []
    end


  fun takeInt s = Int.scan StringCvt.DEC s


  fun takeN n getc strm =
    let
      fun scan 0 l strm = SOME (String.implode (List.rev l), strm)
        | scan i l strm =
          case getc strm of
              NONE => NONE
            | SOME (c, strm) => scan (i - 1) (c::l) strm
    in
      scan n [] strm
    end


  fun takeTill f getc strm =
    let
      fun scan r strm =
          case getc strm of
              NONE => SOME (String.implode (List.rev r), strm)
            | SOME (c, strm') =>
                if f c
                then SOME (String.implode (List.rev r), strm)
                else scan (c::r) strm'
    in
      scan [] strm
    end


  fun takeWhile f getc strm =
    let
      fun scan r strm =
          case getc strm of
              NONE => SOME (String.implode (List.rev r), strm)
            | SOME (c, strm') =>
                if f c
                then scan (c::r) strm'
                else SOME (String.implode (List.rev r), strm)
    in
      scan [] strm
    end


  fun takeTail getc strm =
    let
      fun scan r strm =
          case getc strm of
              NONE => SOME (String.implode (List.rev r), strm)
            | SOME (c, strm) => scan (c::r) strm
    in
      scan [] strm
    end




  fun pure s getc strm = SOME (s, strm)

  fun fail getc strm = NONE

  fun fmap f p = fn getc => fn strm =>
    case p getc strm of
        SOME (r, t) => SOME ((f r), t)
      | NONE        => NONE

  fun bind p pg = fn getc => fn strm =>
    case p getc strm of
        SOME (r, t) => (pg r) getc t
      | NONE        => NONE

  fun apR p1 p2 = fn getc => fn strm =>
    case p1 getc strm of
        SOME (r1, t1) => p2 getc t1
      | NONE          => NONE

  fun apL p1 p2 = fn getc => fn strm =>
    case p1 getc strm of
        SOME (r1, t1) => (case p2 getc t1 of
                              SOME (r2, t2) => SOME (r1, t2)
                            | NONE          => NONE)
      | NONE        => NONE

  fun ap p1 p2 = fn getc => fn strm =>
    case p1 getc strm of
        SOME (r1, t1) => (case p2 getc t1 of
                              SOME (r2, t2) => SOME ((r1 r2), t2)
                            | NONE          => NONE)
      | NONE        => NONE

  fun choice ps = fn getc => fn strm =>
    let
     fun go nil = NONE
       | go (p::ps) =
          case p getc strm of
              SOME (r, t) => SOME (r, t)
            | NONE        => go ps
    in go ps end

  fun many p getc strm =
    case p getc strm of
        NONE => SOME ([], strm)
      | SOME (x, strm) =>
          case many p getc strm of
              NONE => NONE
            | SOME (xs, strm) => SOME (x::xs, strm)

  fun find p getc strm =
    case p getc strm of
        SOME r => SOME r
      | NONE =>
          case getc strm of
              SOME (_, strm) => find p getc strm
            | NONE => NONE


  fun search p getc strm =
    let
      fun scan b p strm =
        case p getc strm of
            SOME (r, strm) => SOME ((String.implode (List.rev b), r), strm)
          | NONE =>
              case getc strm of
                  SOME (c, strm) => scan (c::b) p strm
                | NONE => NONE
    in
      scan [] p strm
    end


  fun f  <$> p  = fmap f p
  fun p  >>= pg = bind p pg
  fun p1  *> p2 = apR p1 p2
  fun p1 <*  p2 = apL p1 p2
  fun p1 <*> p2 = ap  p1 p2

  end

end
