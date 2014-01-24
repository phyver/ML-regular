{
open Parser

let get_string s = String.sub s 1 ((String.length s) - 2)
let get_symbol s = String.sub s 1 ((String.length s) - 1)
let get_index s = int_of_string (String.sub s 1 ((String.length s) - 1))
let get_random s =
    let s = String.sub s 6 ((String.length s) - 6) in
    try int_of_string s
    with Failure _ -> 5
}
let lower_symbol = [ 'a'-'z' ]
let character =  [ 'a'-'z' 'A'-'Z' '0'-'9' '.' ',' ]
let symbol = "`" character
let str = '"' character* '"'
let reg = "R" [ '0'-'9' ]+
let dfa = "D" [ '0'-'9' ]+
let nfa = "N" [ '0'-'9' ]+
let random = "RANDOM" [ '0'-'9' ]*
let line = "-----" "-"*
let num = [ '0'-'9' ]+

rule token = parse
  | '('             { LPAR }
  | "(#"            { LPARHASH }
  | ')'             { RPAR }
  | '['             { LBR }
  | ']'             { RBR }
  | "{I"            { LCURLI }
  | "{D"            { LCURLD }
  | '{'             { LCURL }
  | '}'             { RCURL }
  | '+'             { PLUS }
  | '*'             { STAR }
  | '0'             { ZERO }
  | '1'             { ONE }
  | lower_symbol    { SYMB(Lexing.lexeme_char lexbuf 0) }
  | symbol          { SYMB((Lexing.lexeme lexbuf).[1]) }

  | ','             { COMMA }
  | str             { STR(get_string (Lexing.lexeme lexbuf)) }
  | '/'             { SLASH }
  | '!'             { BANG }
  | '~'             { TILDE }
  | '<'             { LANGL }
  | '>'             { RANGL }
  | "<<"            { LT }
  | ">>"            { GT }
  | "=="            { DOUBLE_EQUAL }
  | '&'             { AMPER }
  | '|'             { PIPE }
  | '.'             { DOT }
  | "TRANS"         { TRANS }
  | "PREF"          { PREF }
  | "EMPTY"         { EMPTY }
  | "INFINITE"      { INFINITE }
  | random          { RANDOM(get_random (Lexing.lexeme lexbuf)) }
  | reg             { REG(get_index (Lexing.lexeme lexbuf)) }
  | dfa             { DFA(get_index (Lexing.lexeme lexbuf)) }
  | nfa             { NFA(get_index (Lexing.lexeme lexbuf)) }
  | ":="            { AFFECT }
  | "->"            { ARROW }
  | "_"             { UNDERSCORE }
  | line            { LINE }
  | '?'             { QUESTION }
  | num             { NUM(int_of_string (Lexing.lexeme lexbuf)) }

  | [' ' '\t']      { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; NEWLINE }
  | eof             { EOF }
  | ":quit"         { QUIT }
  | ":verbose"      { VERBOSE }
  | ":help"         { HELP }
  | ":h"            { HELP }
  | ":quiet"        { QUIET }
  | ":assert"       { ASSERT }
  | "NOT"           { NOT }
  | "{-"            { comments 0 lexbuf }

and comments level = parse
  | "-}"            { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | '\n'            { Lexing.new_line lexbuf; comments level lexbuf }
  | _               { comments level lexbuf }
  | eof             { EOF }

