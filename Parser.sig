local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = string*(int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = int*(int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
in
datatype token =
    ASSIGN of t__1__
  | COMMA of t__2__
  | ELSE of t__3__
  | EOF of t__4__
  | EQUAL of t__5__
  | ID of t__6__
  | IF of t__7__
  | INT of t__8__
  | LESS of t__9__
  | LPAR of t__10__
  | MINUS of t__11__
  | NUM of t__12__
  | PLUS of t__13__
  | RETURN of t__14__
  | RPAR of t__15__
  | SEMICOLON of t__16__
  | THEN of t__17__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> S100.Prog;
