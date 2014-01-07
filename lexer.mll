{
open Parser

let get_string s = String.sub s 1 ((String.length s) - 2)
}
let symbol = [ 'a'-'z' ]
let str = '"' symbol* '"'

rule token = parse
  | '('             { LPAR }
  | ')'             { RPAR }
  | '+'             { PLUS }
  | '*'             { STAR }
  | '0'             { ZERO }
  | '1'             { ONE }
  | symbol          { SYMB(Lexing.lexeme_char lexbuf 0) }

  | ','             { COMMA }
  | str             { STR(get_string (Lexing.lexeme lexbuf)) }
  | '#'             { HASH }
  | '/'             { SLASH }
  | 'D'             { D }
  | '?'             { QUESTION }
  | '~'             { TILDE }
  | "DA"            { DERIVATIVES_AUTOMATON }
  | 'V'             { V }

  | [' ' '\t']      { token lexbuf }
  | '\n'            { NEWLINE }
  | eof             { EOF }
  | 'Q'             { EOF }
  | "{-*"            { comments 0 lexbuf }

and comments level = parse
  | "-}"            { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | _               { comments level lexbuf }
  | eof             { EOF }

