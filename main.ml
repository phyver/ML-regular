
let verbose = ref true

let main () =
    while true
    do
        print_string "> "; flush_all ();
        let lexbuf = Lexing.from_channel stdin in
        try
            Parser.toplevel Lexer.token lexbuf
        with
            | Exit -> print_newline (); exit 0
            | Invalid_argument(_) ->  ()
            | Failure(msg) -> print_string ("problem: " ^ msg ^ "\n")
    done

let _ = Printexc.print main ()
