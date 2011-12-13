local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = char*(int*int)
type t__4__ = (int*int)
type t__5__ = string*(int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = string*(int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = int*(int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = string*(int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = string*(int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
in
datatype token =
    ASSIGN of t__1__
  | CHAR of t__2__
  | CHARCONST of t__3__
  | COMMA of t__4__
  | DEREF of t__5__
  | ELSE of t__6__
  | EOF of t__7__
  | EQUAL of t__8__
  | ID of t__9__
  | IF of t__10__
  | INT of t__11__
  | LBLOCK of t__12__
  | LESS of t__13__
  | LPAR of t__14__
  | LSQRBRACK of t__15__
  | MINUS of t__16__
  | NUM of t__17__
  | PLUS of t__18__
  | RBLOCK of t__19__
  | REF of t__20__
  | RETURN of t__21__
  | RPAR of t__22__
  | RSQRBRACK of t__23__
  | SEMICOLON of t__24__
  | STRINGCONST of t__25__
  | THEN of t__26__
  | WHILE of t__27__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> S100.Prog;
