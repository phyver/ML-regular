open Regexp


(* type of different actions of the toplevel *)
type action =
    | Help                          (* print help *)
    | Simplify of regexp            (* print a simplified form of the regexp *)
    | PrintRaw of regexp            (* print the raw form of the regexp *)
    | Derivative of regexp*string   (* print the simplified form of the derivative *)
    | AllDerivatives of regexp      (* print all the derivatives *)
    | Match of string*regexp        (* match a string against a regexp *)

let do_help () =
    List.iter print_endline
    [
"Commands:";
"  > regexp                 print a simplified form of the regexp";
"  > # regexp               print the raw regexp, for debug purposes";
"  > regexp / \"string\"      print the word derivative of the regexp wrt to the string";
"  > D regexp               print all the word derivatives of the regexp";
"  > \"string\" ~ regexp      matches the string against the regexp";
"  > Q                      quit";
"  > ?                      help message";
    ]

let do_simplify r =
    let r = simplify r in
    print_string "simplified form: "; print_regexp r; print_newline ()

let do_print_raw r =
    print_string "raw regexp: "; print_raw_regexp r; print_newline ()

let do_derivative r s =
    print_string ("'" ^ s ^ "'\n");
    let rd = simplify (word_derivative r s) in
    print_string (s^" derivative: "); print_regexp rd; print_newline ()

let do_all_derivatives r =
    print_string "word derivatives of "; print_regexp r; print_newline ();
    let l = get_all_derivatives r in
    let i = ref 1 in
    List.iter (fun r -> print_int !i; print_string ": "; print_regexp r;
    print_newline (); incr i) l

let do_match r s =
    if match_regexp s r
    then print_string "True\n"
    else print_string "False\n"



