{
open Parser

let get_string s = String.sub s 1 ((String.length s) - 2)
let get_index s = String.sub s 3 ((String.length s) - 3)
let get_state_index s = String.sub s 1 ((String.length s) - 1)
}
let symbol = [ 'a'-'z' ]
let str = '"' symbol* '"'
let reg = "REG" [ '0'-'9' ]+
let dfa = "DFA" [ '0'-'9' ]+
let nfa = "NFA" [ '0'-'9' ]+
let state = "s" [ '0'-'9' ]+
let line = "-----" "-"*

rule token = parse
  | '('             { LPAR }
  | ')'             { RPAR }
  | '['             { LBR }
  | ']'             { RBR }
  | '{'             { LCURL }
  | '}'             { RCURL }
  | '+'             { PLUS }
  | '*'             { STAR }
  | '0'             { ZERO }
  | '1'             { ONE }
  | symbol          { SYMB(Lexing.lexeme_char lexbuf 0) }

  | ','             { COMMA }
  | str             { STR(get_string (Lexing.lexeme lexbuf)) }
  | '#'             { HASH }
  | '/'             { SLASH }
  | '!'             { BANG }
  | '~'             { TILDE }
  | '<'             { LT }
  | '>'             { GT }
  | "=="            { DOUBLE_EQUAL }
  | '&'             { AMPER }
  | '|'             { PIPE }
  | '.'             { DOT }
  | "TRANS"         { TRANS }
  | "PREF"          { PREF }
  | "EMPTY"         { EMPTY }
  | "INFINITE"      { INFINITE }
  | reg             { REG(int_of_string (get_index (Lexing.lexeme lexbuf))) }
  | dfa             { DFA(int_of_string (get_index (Lexing.lexeme lexbuf))) }
  | nfa             { NFA(int_of_string (get_index (Lexing.lexeme lexbuf))) }
  | state           { STATE(int_of_string (get_state_index (Lexing.lexeme lexbuf))) }
  | ":="            { AFFECT }
  | "->"            { ARROW }
  | "_"             { UNDERSCORE }
  | line            { LINE }

  | [' ' '\t']      { token lexbuf }
  | '\n'            { NEWLINE }
  | eof             { EOF }
  | ":q"            { QUIT }
  | ":quit"         { QUIT }
  | ":v"            { VERBOSE }
  | ":verbose"      { VERBOSE }
  | ":h"            { HELP }
  | ":help"         { HELP }
  | ":a"            { ASSERT }
  | ":assert"       { ASSERT }
  | "NOT"           { NOT }
  | "{-"            { comments 0 lexbuf }

and comments level = parse
  | "-}"            { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | _               { comments level lexbuf }
  | eof             { EOF }

