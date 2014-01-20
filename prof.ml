let concat_n s n =
    let rec aux s n acc = if n<1 then acc else aux s (n-1) (s^acc)
    in aux s n ""

let main () =
    let n = 64 in
    let cmd = "\"" ^ (concat_n "a" n) ^ "\" < " ^ (concat_n "(1+a)" n) ^ (concat_n "a" n) in
    print_endline cmd;
    let lexbuf = Lexing.from_string (cmd^"\n") in
        Parser.toplevel Lexer.token lexbuf

let _ = Printexc.print main ()
