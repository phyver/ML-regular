(***************************************************************)
(*  Copyright 2014 Pierre Hyvernat. All rights reserved.       *)
(*  This file is distributed under the terms of the            *)
(*  GNU General Public License, described in file COPYING.     *)
(***************************************************************)

{
open Parser

let get_symbol s = String.sub s 1 ((String.length s) - 1)
let get_index s = int_of_string (String.sub s 1 ((String.length s) - 1))
let get_random s =
    let s = String.sub s 6 ((String.length s) - 6) in
    try int_of_string s
    with Failure _ -> 5
}
let character =  [ 'a'-'z' ]
let reg = "R" [ '0'-'9' ]+
let dfa = "D" [ '0'-'9' ]+
let nfa = "N" [ '0'-'9' ]+
let random = "RANDOM" [ '0'-'9' ]*
let line = "-----" "-"*
let num = [ '0'-'9' ]+
let help = ":help" | ":h"
let spaces = [ ' ' '\t' ]+

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
  | "^*"            { STAR }  (* to allow regexp pasted from TeX source *)
  | '0'             { ZERO }
  | '1'             { ONE }
  | character       { SYMB(Lexing.lexeme_char lexbuf 0) }

  | ','             { COMMA }
  | '/'             { SLASH }
  | '\\'            { BACKSLASH }
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
  | "TRANSPOSE"     { TRANS }
  | "PREFIX"        { PREF }
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
  | "IN"            { IN }
  | '"'             { QUOTE }

  | [' ' '\t']      { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; NEWLINE }
  | eof             { EOF }
  | ":quit"         { QUIT }
  | ":set"          { SET }
  | "verbose"       { VERBOSE }
  | "quiet"         { QUIET }
  | "alphabet"      { ALPHABET }
  | help spaces "word"     { HELP_WORD }
  | help spaces "regexp"   { HELP_REGEXP }
  | help spaces "dfa"      { HELP_DFA }
  | help spaces "nfa"      { HELP_NFA }
  | help            { HELP }
  | ":assert"       { ASSERT }
  | ":derivatives"  { DERIVATIVES }
  | "NOT"           { NOT }
  | "{-"            { comments 0 lexbuf }

and comments level = parse
  | "-}"            { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "{-"            { comments (level+1) lexbuf }
  | '\n'            { Lexing.new_line lexbuf; comments level lexbuf }
  | _               { comments level lexbuf }
  | eof             { EOF }

