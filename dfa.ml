open Misc

(***
 *** deterministic automata
 ***)

(* types for states and symbols *)
type symbol = char
type state = string

(* type for deterministic automata (the transition function may raise Not_found) *)
type dfa = {
    init : state                            ;
    transition : state -> symbol -> state   ;
    accepting : state -> bool               ;

    all_states : state list                 ;
    all_symbols : symbol list              ;
}

(* transform an association list into a dfa *)
let make_dfa (init : state)
             (matrix : (state * ((symbol*state) list)) list)
             (final : state list)
    : dfa =

    let all_symbols = List.concat (List.map (fun row -> List.map fst (snd row)) matrix) in
    let all_symbols = List.sort compare all_symbols in
    let all_symbols = uniq all_symbols in

    let all_states = List.concat (List.map (fun row -> List.map snd (snd row)) matrix) in
    let all_states = List.rev_append (List.map fst matrix) all_states in
    let all_states = List.sort compare all_states in
    let all_states = uniq all_states in

    let transition s a = List.assoc a (List.assoc s matrix) in
    let accepting s = List.mem s final in
    {
        init = init                     ;
        transition = transition         ;
        accepting = accepting           ;
        all_states = all_states         ;
        all_symbols = all_symbols       ;
    }


(* check if a dfa is total *)
let total (d:dfa) : bool =
    try
        List.mem d.init d.all_states &&
        List.for_all
        (fun s ->
            List.for_all
            (fun a -> List.mem (d.transition s a) d.all_states
            )
            d.all_symbols
        )
        d.all_states
    with Not_found -> false


(* compute the list of reachable states *)
let reachable (d:dfa) : state list =
    let rec dfs current visited =
        if List.mem current visited
        then visited
        else List.fold_left (fun visited a -> try dfs (d.transition current a) visited
                                              with Not_found -> visited) visited d.all_symbols
    in
    uniq (List.sort compare (dfs d.init []))


(* number of nodes *)
let cardinal (d:dfa) : int = List.length d.all_states


(* check if automaton accepts a word *)
let accepts (d:dfa) (w:string) : bool =
    let rec aux s l = match l with
        | [] -> d.accepting s
        | a::l -> aux (d.transition s a) l
    in
    aux d.init (explode w)


(* print an automaton *)
let print_dfa (d:dfa) (show_labels:bool) : unit =

    let symbols = d.all_symbols in
    let states = d.all_states in
    let width = if show_labels
                then List.fold_left (fun w s -> max w (String.length s)) 0 states
                else String.length (string_of_int (List.length states))
    in

    let print_row s =
        (* state *)
        if s = d.init
        then print_string "-> "
        else print_string "   ";
        if show_labels
        then print_string_w s width
        else print_string_w (string_of_int (Misc.idx s states)) width;
        if d.accepting s
        then print_string " -> | "
        else print_string "    | ";

        (* transitions *)
        List.iter
            (fun a ->
                let t = try
                            if show_labels
                            then d.transition s a
                            else string_of_int (idx (d.transition s a) states)
                        with Not_found -> ""
                in
                print_string_w t (1+width)
            )
            symbols;

        print_newline()
    in

    print_spaces (6+width);
    print_string " | ";
    List.iter
        (fun a -> print_char_w a (1+width))
        symbols;
    print_newline ();
    print_line (9+width+(1+width)*(List.length symbols));

    print_newline();

    List.iter print_row states


(* from regexp to dfa *)
let dfa_from_regexp (r:Regexp.regexp) : dfa =

    let all_symbols = Regexp.get_symbols r in

    let init = Regexp.string_of_regexp r in

    let rec add_transition_row a t row =
        try
            let _ = List.assoc a row in
            assert false
        with Not_found -> (a,t)::row
    in

    let rec add_transition s a t matrix acc =
        match matrix with
        | [] -> (s,[a,t])::acc
        | (st,row)::matrix when st=s -> (s, add_transition_row a t row)::(List.rev_append acc matrix)
        | (st,row)::matrix -> add_transition s a t matrix ((st,row)::acc)
    in

    let rec aux done_states matrix accepting todo =
        match todo with
        | [] -> done_states, matrix, accepting
        | r::todo ->
                let str = Regexp.string_of_regexp r in
                let accepting = if (Regexp.contains_epsilon r) then str::accepting else accepting in
                if List.mem str done_states
                then aux done_states matrix accepting todo
                else
                    let matrix,todo =
                        List.fold_left
                            (fun mt a ->
                                let matrix, todo = mt in
                                let ra = Regexp.simplify (Regexp.derivative r a) in
                                let stra = Regexp.string_of_regexp ra in
                                let matrix = add_transition str a stra matrix [] in
                                let todo = ra::todo in
                                matrix, todo
                            )
                            (matrix,todo)
                            all_symbols
                    in
                    aux (str::done_states) matrix accepting todo
    in

    let done_states, matrix, accepting = aux [] [] [] [r] in

    make_dfa init matrix accepting

