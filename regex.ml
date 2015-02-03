(***************************************************************)
(*  Copyright 2014 Pierre Hyvernat. All rights reserved.       *)
(*  This file is distributed under the terms of the            *)
(*  GNU General Public License, described in file COPYING.     *)
(***************************************************************)

open Common


(***
 *** regular expressions and their derivatives
 ***)

(* type for symbols *)
type symbol = char
type var = string

(* type for basic regular expressions *)
type regex =
  (* basic regex *)
  | Zero
  | One
  | Symb of symbol
  | Sum of regex*regex
  | Product of regex*regex
  | Star of regex
  | Neg of regex
  | Var of var

(* a (context free) language is given by a list of regular equations *)
type language = (var * regex) list

(***
 *** printing and related
 ***)
let rec string_of_regex (r:regex) : string = match r with
    | Zero -> "0"
    | One -> "1"
    | Symb(a) ->
            if String.contains "abcdefghijklmnopqrstuvwxyz" a
            then String.make 1 a
            else "`" ^ String.make 1 a
    | Star(Zero as r) | Star(One as r) | Star(Symb(_) as r) ->
            (string_of_regex r) ^ "*"
    | Star(r) -> "(" ^ (string_of_regex r) ^ ")*"
    | Sum(r1, r2) -> (string_of_regex r1) ^ " + " ^ (string_of_regex r2)
    | Product((Sum(_) as r1), (Sum(_) as r2)) ->
            "(" ^ (string_of_regex r1) ^")(" ^ (string_of_regex r2) ^")"
    | Product((Sum(_) as r1), r2) ->
            "(" ^ (string_of_regex r1) ^")" ^ (string_of_regex r2)
    | Product(r1, (Sum(_) as r2)) ->
            (string_of_regex r1) ^"(" ^ (string_of_regex r2) ^")"
    | Product(r1, r2) ->
            (string_of_regex r1) ^ (string_of_regex r2)
    | Neg(Zero as r) | Neg(One as r) | Neg(Symb(_) as r) -> "~" ^ (string_of_regex r)
    | Neg(r) -> "~(" ^ (string_of_regex r) ^ ")"
    | Var(s) -> "[" ^ s ^ "]"


(* main printing function *)
let rec print_regex (r:regex) : unit =
    print_string (string_of_regex r)


(* print a context free language *)
let print_language (l:language) : unit =
    List.iter
        (function x,r ->
            print_string x;
            print_string " -> ";
            print_regex r;
            print_newline())
        l

(* print the raw regex, with parenthesis everywhere *)
let rec print_raw_regex (r:regex) : unit = match r with
    | Zero -> print_string "0"
    | One -> print_string "1"
    | Symb(a) -> print_char a
    | Star(Zero as r) | Star(One as r) | Star(Symb(_) as r) ->
            print_raw_regex r; print_string "*"
    | Star(r) -> print_string "("; print_raw_regex r; print_string ")*"
    | Sum(r1, r2) ->
            print_string "(" ; print_raw_regex r1; print_string " + ";
            print_raw_regex r2 ; print_string ")"
    | Product(r1, r2) ->
            print_string "(" ; print_raw_regex r1; print_string "." ;
            print_raw_regex r2 ; print_string ")"
    | Neg(r) -> print_string "~(" ; (print_raw_regex r) ; print_string ")"
    | Var(s) -> print_string s


(***
 *** simplifying a regex
 ***)

(* get all top-level summands from a regex *)
let rec get_summands (r:regex): regex list = match r with
    | Zero -> []
    | Sum(r1, r2) -> List.rev_append (get_summands r1) (get_summands r2)
          (* addition is commutative, so that the order is unimportant *)
    | r -> [r]

(* its converse: convert a list into a sum *)
let rec list2sum (l:regex list) :regex = match l with
    | [] -> Zero
    | [r] -> r
    | r::l -> Sum(r, list2sum l)

(* get all top-level factors from a regex *)
let rec get_factors (r:regex): regex list = match r with
    | One -> []
    | Product(r1, r2) -> List.append (get_factors r1) (get_factors r2)
    | r -> [r]

(* its converse: convert a list into a product *)
let rec list2product (l:regex list) : regex = match l with
    | [] -> One
    | [r] -> r
    | r::l -> Product(r, list2product l)

(* simplify a toplevel sum, without recursion *)
let simplify_product (r1:regex) (r2:regex) : regex =
    let l = get_factors (Product(r1,r2)) in
    let l = List.filter (fun x -> x <> One) l in
    if List.mem Zero l
    then Zero
    else list2product l

(* simplify a toplevel product, without recursion *)
let simplify_sum (r1:regex) (r2:regex) : regex =
    let l = get_summands (Sum(r1,r2)) in
    let l = List.filter (fun x -> x <> Zero) l in
    let l = List.sort compare l in
    let l = uniq l in
    if List.mem (Neg(Zero)) l
    then Neg(Zero)
    else list2sum l

(* simplify a regex recursively *)
let rec simplify (r:regex) : regex = match r with
    | One | Zero | Symb(_) | Var(_) -> r
    | Star(r) ->
            begin
                let r = simplify r in
                match r with
                  | Zero | One -> One
                  | Star(r) -> Star(r)
                  | _ -> Star(simplify r)
            end
    | Product(r1, r2) ->
            let r1 = simplify r1 in
            let r2 = simplify r2 in
            simplify_product r1 r2
    | Sum(r1, r2) ->
            let r1 = simplify r1 in
            let r2 = simplify r2 in
            simplify_sum r1 r2
    | Neg(r) -> let r = simplify r in (match r with Neg(r) -> r | r -> Neg(r))


(***
 *** derivatives and related
 ***)

(* check if the empty string belongs to a context-free language *)
let contains_epsilon_language (l:language) (r:regex) : bool =
    (* we keep a list of variables we've already seen to avoid infinite
     * recursion: we try to find a finite path (without loop) that is equal
     * to 1... The reference "visited" makes it possible to forbid loops.
     *)
    let visited = ref [] in
    let rec aux r =
        match r with
            | One -> true
            | Zero -> false
            | Symb(_) -> false
            | Sum(r1, r2) -> aux r1 || aux r2
            | Product(r1, r2) -> aux r1 && aux r2
            | Star r -> true
            | Neg(r) ->
                    begin
                        match l with
                            | [] -> not (aux r)
                            | _ -> assert false
                    end
            | Var(s) ->
                    begin
                        match l with
                            | [] -> assert false
                            | _ -> if List.mem s !visited
                                   then false
                                   else (visited:=s::!visited;
                                         aux (List.assoc s l))
                    end
    in
    aux r

let contains_epsilon (r:regex) : bool =
    contains_epsilon_language [] r

(* the "derivative with respect to a symbol" of a regular expression *)
module MP = Map.Make(struct type t=regex let compare = compare end)
module MS = Map.Make(struct type t=string let compare = String.compare end)
module SS = Set.Make(struct type t=string let compare = String.compare end)
let language_derivative (l:language) (x:var) (a:symbol) : language =
    let initial_language = List.fold_left (fun l xr -> MS.add (fst xr) (snd xr) l) MS.empty l in
    let derived_language = ref (MS.empty) in
    let to_derive = ref (SS.singleton x) in

    let rec get_var r = match r with
        | Zero | One | Symb(_) -> ()
        | Sum(r1,r2) | Product(r1,r2) -> get_var r1; get_var r2
        | Star(r) | Neg(r) -> get_var r
        | Var(x) ->
                if MS.mem x initial_language
                then derived_language := MS.add x (MS.find x initial_language) !derived_language
    in

    let mem = ref MP.empty in
    let rec derivative_mem (r:regex) (a:symbol) : regex =
        try MP.find r !mem
        with Not_found ->
            let d = match r with
                | Zero | One -> Zero
                | Symb(b) when b = a -> One
                | Symb(_) -> Zero
                | Sum(r1, r2) -> simplify_sum (derivative_mem r1 a)
                                              (derivative_mem r2 a)
                | Product(r1, r2) ->
                        if contains_epsilon_language l r1
                        then
                            let p = simplify_product (derivative_mem r1 a)
                                                     r2
                            in simplify_sum p (derivative_mem r2 a)
                        else
                            simplify_product (derivative_mem r1 a) r2
                | Star(r) -> simplify_product (derivative_mem r a)
                                              (Star(r))
                | Neg(_) -> raise (Failure "negation not allowed in context-free languages")
                | Var(x) ->
                        begin
                            let xa = x ^ (String.make 1 a) in
                            if not (MS.mem xa !derived_language)
                            then to_derive := SS.add x !to_derive;
                            Var(xa)
                        end
            in
            mem := MP.add r d !mem;
            d
    in


    try
        while true
        do
            if SS.is_empty !to_derive
            then raise Exit;

            let x = SS.choose !to_derive in
            let xa = x ^ (String.make 1 a) in
            to_derive := SS.remove x !to_derive;
            let d = derivative_mem (MS.find x initial_language) a in
            get_var d;
            derived_language := MS.add xa d !derived_language

        done;
        assert false
    with Exit ->
        MS.fold (fun x r acc -> (x,r)::acc ) !derived_language []


(* the "derivative with respect to a symbol" of a regular expression *)
(* module MP = Map.Make(struct type t=regex let compare = compare end) *)
let derivative (r:regex) (a:symbol) : regex =
    (* we'll memoize the results *)
    let mem = ref MP.empty in
    let rec derivative_mem (r:regex) (a:symbol) : regex =
        try MP.find r !mem
        with Not_found ->
            let d = match r with
                | Zero | One -> Zero
                | Symb(b) when b = a -> One
                | Symb(_) -> Zero
                | Sum(r1, r2) -> simplify_sum (derivative_mem r1 a)
                                              (derivative_mem r2 a)
                | Product(r1, r2) ->
                        if contains_epsilon_language [] r1
                        then
                            let p = simplify_product (derivative_mem r1 a)
                                                     r2
                            in simplify_sum p (derivative_mem r2 a)
                        else
                            simplify_product (derivative_mem r1 a) r2
                | Star(r) -> simplify_product (derivative_mem r a)
                                              (Star(r))
                | Neg(r) -> Neg (derivative_mem r a)
                | Var(_) -> raise (Failure "cannot derive regex variable outside context-free languages")
            in
            mem := MP.add r d !mem;
            d
    in
    derivative_mem r a

(* the derivative with respect to a word *)
let word_derivative (r:regex) (w:string) : regex =
    let n = String.length w in
    let rec aux r i =
        if i = n
        then r
        else aux (derivative r w.[i]) (i+1)
    in
    aux r 0


(* match a string against a regex using iterated derivatives *)
let match_regex (w:string) (r:regex) : bool =
    contains_epsilon (word_derivative r w)


(* the "constant part" of a regex *)
let constant_part (r:regex) : regex =
  if contains_epsilon r
  then One
  else Zero


let language_word_derivative (l:language) (x:var) (w:string) : language =
    let n = String.length w in
    let rec aux l x i =
        if i = n
        then l
        else
            let a = w.[i] in
            let xa = x ^ (String.make 1 a) in
            aux (language_derivative l x a) xa (i+1)
    in
    aux l x 0


(* checking if a work belongs to a context-free language using derivatives *)
let match_language (w:string) (l:language) (x:var) : bool =
    let n = String.length w in
    let rec aux i l x =
        if i = n
        then contains_epsilon_language l (Var(x))
        else
            let a = w.[i] in
            let xa = x ^ (String.make 1 a) in
            let l = language_derivative l x a in
(*                 print_endline "new derivative: "; print_language l; print_newline(); *)
(*                 print_endline ((string_of_int (List.length l)) ^ " equations"); *)
            aux (i+1) l xa
    in
    aux 0 l x


(***
 *** misc functions on regexs
 ***)

(* get a list of symbols used in a regex *)
let get_symbols (r:regex) : symbol list =
    let rec aux r = match r with
    | One | Zero -> []
    | Symb(a) -> [a]
    | Star(r) -> aux r
    | Sum(r1, r2) | Product(r1, r2) -> List.rev_append (aux r1) (aux r2)
    | Neg(r) -> aux r
    | Var(_) -> []
    in
    let l = aux r in
    let l = List.sort compare l in
    let l = uniq l in
    l


(* compute the list of all possible iterated derivatives of a regex *)
let get_all_derivatives (r:regex) : (regex * (symbol list)) list =
    let symbols = get_symbols r in

    let rec union l1 l2 = match l2 with
       | [] -> l1
       | (r,w)::l2 -> if List.mem_assoc r l1
                      then union l1 l2
                      else union ((r,w)::l1) l2
    in

    let rec aux ok todo = match todo with
        | [] -> List.rev ok
        | (r,w)::todo ->
                if List.mem_assoc r ok
                then aux ok todo
                else
                    let ld = List.map (fun a -> (simplify (derivative r a), a::w)) symbols in
                    let todo = union todo ld in
                    aux ((r,w)::ok) todo
   in
   aux [] [(simplify r, [])]


(** transpose *)
let rec transpose (r:regex) : regex = match r with
    | Zero | One | Symb(_) -> r
    | Sum(r1,r2) -> Sum(transpose r1, transpose r2)
    | Product(r1,r2) -> Product(transpose r2, transpose r1)
    | Star(r) -> Star(transpose r)
    | Neg(r) -> Neg(transpose r)
    | Var(s) -> Var(s ^ "^t")


(* antiderivative *)
let word_antiderivative (r:regex) (w:string) : regex =
    let rec aux r i =
        if i=0
        then transpose r
        else aux (derivative r w.[i-1]) (i-1)
    in
    let tr = transpose r in
    aux tr (String.length w)


(* check if a regex denotes the empty language *)
let rec is_empty (r:regex) : bool = match r with
    | Zero -> true
    | One | Symb(_) -> false
    | Sum(r1,r2) -> is_empty r1 && is_empty r2
    | Product(r1,r2) -> is_empty r1 || is_empty r2
    | Star(_) -> false
    | Neg(r) -> not (is_empty r)
    | Var(_) -> raise (Failure "regex variable are allowed outside context free languages")


(* check if the languae of a regular is less than One, ie it contains at most
 * the empty word *)
let rec lessOne (r:regex) : bool = match r with
    | Zero | One -> true
    | Symb(_) -> false
    | Sum(r1,r2) -> lessOne r1 && lessOne r2
    | Product(r1,r2) -> is_empty r1 || is_empty r2 || (lessOne r1 && lessOne r2)
    | Star(r) -> lessOne r
    | Neg(_) -> raise (Failure "cannot check directly if a complement regex is less than One")
    | Var(_) -> raise (Failure "regex variable are allowed outside context free languages")


(* check if the language of a regex is infinite *)
let rec is_infinite (r:regex) : bool = match r with
    | Zero | One | Symb(_) -> false
    | Sum(r1,r2) -> is_infinite r1 || is_infinite r2
    | Product(r1,r2) -> (is_infinite r1 && not (is_empty r2)) ||
                        (not (is_empty r1) && is_infinite r2)
    | Star(r) -> not (lessOne r)
    | Neg(_) -> not (is_infinite r)
    | Var(_) -> raise (Failure "regex variable are allowed outside context free languages")


(* compute the regex of prefixes *)
let rec prefix (r:regex) : regex = match r with
    | Zero -> Zero
    | One -> One
    | Symb(a) -> Sum(One,Symb(a))
    | Sum(r1,r2) -> Sum(prefix r1, prefix r2)
    | Product(r1,r2) ->
            let p = simplify_product r1 (prefix r2) in
            simplify_sum (prefix r1) p
    | Star(r) -> simplify_product (Star(r)) (prefix r)
    | Neg(_) -> raise (Failure "cannot compute directly the prefix of a complement regex")
    | Var(_) -> raise (Failure "regex variable are allowed outside context free languages")


(* random regex of (at most) given depth using a given alphabet *)
let rec random_regex ?(alphabet=['a';'b';'c';'d';'e']) n =
    if n < 1
    then
        begin
            match Random.int 9 with
                | 0 -> Zero
                | 1 | 2 | 3 -> One
                | 4 | 5 | 6 | 7 | 8 -> Symb(List.nth alphabet (Random.int (List.length alphabet)))
                | _ -> assert false
        end
    else
        begin
            match Random.int 33 with
                | 0 -> Zero
                | 1 | 2 | 3 -> One
                | 4 | 5 | 6 | 8  -> Symb(List.nth alphabet (Random.int (List.length alphabet)))
                | x when x < 17 -> Sum(random_regex (n-1), random_regex (n-1))
                | x when x < 25 -> Product(random_regex (n-1), random_regex (n-1))
                | x when x < 33 -> Star(random_regex (n-1))
                | _ -> assert false
        end

