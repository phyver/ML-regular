
let verbose = ref true

let read_file f =
    let ch = open_in f in
    print_endline ("reading file " ^ f);
    try
        let lexbuf = Lexing.from_channel ch in
        while true
        do
            begin
            try
                Parser.toplevel Lexer.token lexbuf
            with
                | Invalid_argument(s) ->  print_endline s
                | Failure(msg) -> print_endline ("problem: " ^ msg); Lexing.flush_input lexbuf
                | Parsing.Parse_error -> print_endline "parse error"
            end;
        done
    with End_of_file -> ()


let main () =
    Array.iter (fun f ->
        try
            read_file f
        with
            | Sys_error(er) -> print_endline er; exit 1
    ) (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1));

    let lexbuf = Lexing.from_channel stdin in
    while true
    do
        print_string "> "; flush_all ();
        try
            Parser.toplevel Lexer.token lexbuf;
            flush_all()
        with
            | End_of_file -> print_newline (); exit 0
            | Invalid_argument(s) ->  print_endline s
            | Failure(msg) -> print_endline ("problem: " ^ msg); Lexing.flush_input lexbuf
            | Parsing.Parse_error -> print_endline "parse error"
    done


let _ = Printexc.print main ()
