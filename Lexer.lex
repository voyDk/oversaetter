{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "if"           => Parser.IF pos
       | "else"         => Parser.ELSE pos
       | "while"	=> Parser.WHILE pos
       | "int"          => Parser.INT pos
       | "char"		=> Parser.CHAR pos
       | "return"       => Parser.RETURN pos
       | _              => Parser.ID (s, pos)

 (* removes quotes from string s *)
 (* used to correct input to Char/String.fromCString *)
 fun removeQuotes s = String.substring(s,1,String.size(s)-2); 
 
 (*fun removeStar s = String.substring(s,1,String.size(s)-1);*)

 fun removeStar s = String.implode (List.filter (fn x => not (x = #"*")) (String.explode s))
}


rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
    | "/*" ([^`*`] | `*`[^`/`])* "*/"
			{ Token lexbuf } (* comment *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer"
                             | SOME i => Parser.NUM (i, getPos lexbuf) }
  | `'` ((`\`?[`a`-`z` `A`-`Z` `0`-`9` `!` `#` `$` `%` `&` `(` `)` `*` `+` `,` `-` `.` `/` `:` `;` `<` `=` `>` `?` `@` `[` `]` ``` `^` `_` `\`` `{` `|` `}` `~` `]` ` `]) | (`\`[`'` `"` `\`])) `'`
			{ case Char.fromCString (removeQuotes(getLexeme lexbuf)) of
			       NONE   => lexerError lexbuf "Bad CharConst"
			     | SOME c => Parser.CHARCONST (c, getPos lexbuf) }

  | `"`[`a`-`z` `A`-`Z` `0`-`9` `!` `#` `$` `%` `&` `(` `)` `*` `+` `,` `-` `.` `/` `:` `;` `<` `=` `>` `?` `@` `[` `]` `^` `_` `\`` `{` `|` `}` `~` `]` `'` ` `]*`"`
			{ case String.fromCString (removeQuotes(getLexeme lexbuf)) of
			       NONE   => lexerError lexbuf "Bad StringConst"
			     | SOME s => Parser.STRINGCONST (s, getPos lexbuf) }
  | `*`[`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { Parser.REF (removeStar(getLexeme lexbuf), getPos lexbuf) }
  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*`*`
                        { Parser.DEREF (removeStar(getLexeme lexbuf), getPos lexbuf) }
  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | `<`                 { Parser.LESS (getPos lexbuf) }
  | `=`                 { Parser.ASSIGN (getPos lexbuf) }
  | "=="                { Parser.EQUAL (getPos lexbuf) }
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `{`                 { Parser.LBLOCK (getPos lexbuf) }
  | `}`                 { Parser.RBLOCK (getPos lexbuf) }
  | `[`                 { Parser.LSQRBRACK (getPos lexbuf) }
  | `]`                 { Parser.RSQRBRACK (getPos lexbuf) }
  | `,`                 { Parser.COMMA (getPos lexbuf) }
  | `;`                 { Parser.SEMICOLON (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
