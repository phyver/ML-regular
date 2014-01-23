
let read_channel ch prompt =
    try
        let lexbuf = Lexing.from_channel ch in
        while true
        do
            if prompt
            then (print_string "# "; flush_all ());
            try
                Parser.toplevel Lexer.token lexbuf;
                flush_all()
            with
                | Invalid_argument(s) ->  print_endline s
                | Failure(msg) -> print_endline ("*** problem: " ^ msg); Lexing.flush_input lexbuf
                | Parsing.Parse_error -> print_endline "*** parse error"; Lexing.flush_input lexbuf
        done
    with
        | End_of_file -> print_newline()


let main () =
    Random.self_init ();
    print_endline "                      ML-regular";
    print_newline ();

    Array.iter (fun f ->
        try
            print_endline (">>> reading file " ^ f);
            let ch = open_in f in
            read_channel ch false;
        with
            | Sys_error(er) -> print_endline er; exit 1
    ) (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1));
    read_channel stdin true


let _ = Printexc.print main ()
