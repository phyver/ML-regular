open Regexp
open Utils

let main () =
    while true
    do
        print_string "> "; flush_all ();
        let lexbuf = Lexing.from_channel stdin in
        try
            match Parser.toplevel Lexer.token lexbuf with
            | Simplify(r) -> do_simplify r
            | PrintRaw(r) -> do_print_raw r
            | Derivative(r, s) -> do_derivative r s
            | AllDerivatives(r) -> do_all_derivatives r
            | Match(s,r) -> do_match r s
            | Help -> do_help ()

        with
            | Exit -> print_newline (); exit 0
            | Invalid_argument("empty") ->  ()
            | Failure(msg) -> print_string ("problem: " ^ msg ^ "\n")
    done

let _ = Printexc.print main ()
