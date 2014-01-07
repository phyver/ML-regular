(***
 *** deterministic automata
 ***)

open Misc

(* types for states and symbols *)
type symbol = char
type state = string

(* type for deterministic automata (the transition function may raise Not_found) *)
type dfa = {
    init : state                            ;
    transition : state -> symbol -> state   ;
    accepting : state -> bool               ;

    all_states : state list                 ;
    all_symbols : symbol list               ;
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
        else List.fold_left (fun visited a -> try dfs (d.transition current a) (current::visited)
                                              with Not_found -> visited) visited d.all_symbols
    in
    uniq (List.sort compare (dfs d.init []))


(* restrict a DFA to its reachable states *)
let restrict_reachable (d:dfa) : dfa =
    {
        init = d.init;
        transition = d.transition;
        accepting = d.accepting;
        all_states = reachable d;
        all_symbols = d.all_symbols;
    }


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

    print_n_char ' ' (6+width);
    print_string " | ";
    List.iter
        (fun a -> print_char_w a (1+width))
        symbols;
    print_newline ();
    print_n_char '-' (9+width+(1+width)*(List.length symbols));

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


(* minimisation *)
let minimize (d:dfa) : dfa =
    let states = List.sort compare (reachable d) in
    let symbols = List.sort compare (d.all_symbols) in

    (* get the list of pairs (x,y) with x>y, where x and y are in l *)
    let rec square l acc = match l with
        | [] -> acc
        | x::l -> square l (List.rev_append (List.map (fun y -> (max x y, min x y)) l) acc)
    in

    (* all the pairs of states *)
    let pairs = square states [] in

    (* xor function: exactly one of a and b is true *)
    let xor a b = (a && not b) || (not a && b) in

    (* "different" is the list of pairs of _surely_ different states,
     * "similar" is the list of pairs of _maybe_ similar states *)
    let different, similar = List.partition (function x,y -> xor (d.accepting x) (d.accepting y)) pairs in

    (* at each step, we want to look among the _maybe_ similar state to see of
     * some of them are in fact _surely_ different: this happens when they
     * have _surely_ different neighboors.
     *   - "different" is the current list of _surely_ different states
     *   - "similar" is the current list of _maybe_ similar states we've
     *      already checked at this step
     *   - "tocheck" is the list of _maybe_ similar states we still need to
     *      check at this step
     *   - "change" is a boolean that records if we have changed some pair
     *     from "tocheck" to "different"
     *)
    let rec one_step different similar tocheck change = match tocheck with
      | [] -> different, similar, change
      | (x,y)::tocheck ->
              let b = List.exists
                        (fun a ->
                          let xa = d.transition x a in
                          let ya = d.transition y a in
                          let xya = (max xa ya, min xa ya) in
                            List.mem xya different)
                        symbols
              in
              if b
              then one_step ((x,y)::different) similar tocheck true
              else one_step different ((x,y)::similar) tocheck change
    in

    (* we call the previous function until no more pair from "similar" passes
     * into the list of pairs "different"
     * The list "similar" should then correspond to an equivalence relation *)
    let rec all_steps different similar =
        let different, similar, change = one_step different [] similar false in
        if change
        then all_steps different similar
        else similar
    in

    (* equiv is now the equivalence relation of similarity between states *)
    let equiv = List.sort compare (all_steps different similar) in

    (*
    let check_is_equiv relation =
        List.iter (fun x ->
        List.iter (fun y ->
        List.iter (fun z ->
            if List.mem (min x y, max x y) relation &&
               List.mem (min x z, max x z) relation
            then if y=z || (List.mem (min y z, max y z) relation)
                 then ()
                 else print_endline ("problem:\n   " ^ x ^ "\n   " ^ y ^ "\n   " ^ z ^ "\n"))
        states)
        states)
        states in
    let _ = check_is_equiv equiv in
    *)

    (* an association list that give, for each state, the smallest element in
     * its equivalence classe
     * we suppose that the list "states" is sorted, and that the list "equiv"
     * contains pairs "x,y" with "x>y", and that it is sorted with the
     * lexicographic order
     * the function is similar to the usual merge function *)
    let representants =
        let rec aux equiv states acc = match equiv,states with
        | [], states -> List.rev_append acc (List.combine states states)
        | equiv, [] -> List.rev acc
        | (x1,y1)::equiv, x2::states when x1=x2 ->
                aux equiv states ((x1,min y1 x2)::acc)
        | (x1,y1)::equiv, x2::states when x1<x2 ->
                aux equiv (x2::states) acc
        | (x1,y1)::equiv, x2::states when x1>x2 ->
                aux ((x1,y1)::equiv) states ((x2,x2)::acc)
        | _ -> assert false
        in
        aux equiv states []
    in
    let repr s = List.assoc s representants in

    (* the states of the quotiented DFA are the representants *)
    let quotient_states = List.map snd representants in
    let quotient_states = List.sort compare quotient_states in
    let quotient_states = uniq quotient_states in

    {
        init = repr d.init                                  ;
        transition = (fun s a -> repr (d.transition s a))   ;
        accepting = d.accepting                             ;
        all_states = quotient_states                        ;
        all_symbols = symbols                               ;
    }


(* complementation *)
let complement (d:dfa) : dfa =
    {
        init = d.init                               ;
        transition = d.transition                   ;
        accepting = (fun s -> not (d.accepting s))  ;
        all_states = d.all_states                   ;
        all_symbols = d.all_symbols                 ;
    }

(* TODO: union, intersection *)
