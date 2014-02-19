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
type regexp =
  (* basic regexp *)
  | Zero
  | One
  | Symb of symbol
  | Sum of regexp*regexp
  | Product of regexp*regexp
  | Star of regexp
  | Neg of regexp
  | Var of var

(* a (context free) language is given by a list of regular equations *)
type language = (var * regexp) list

(***
 *** printing and related
 ***)
let rec string_of_regexp (r:regexp) : string = match r with
    | Zero -> "0"
    | One -> "1"
    | Symb(a) ->
            if String.contains "abcdefghijklmnopqrstuvwxyz" a
            then String.make 1 a
            else "`" ^ String.make 1 a
    | Star(Zero as r) | Star(One as r) | Star(Symb(_) as r) ->
            (string_of_regexp r) ^ "*"
    | Star(r) -> "(" ^ (string_of_regexp r) ^ ")*"
    | Sum(r1, r2) -> (string_of_regexp r1) ^ " + " ^ (string_of_regexp r2)
    | Product((Sum(_) as r1), (Sum(_) as r2)) ->
            "(" ^ (string_of_regexp r1) ^")(" ^ (string_of_regexp r2) ^")"
    | Product((Sum(_) as r1), r2) ->
            "(" ^ (string_of_regexp r1) ^")" ^ (string_of_regexp r2)
    | Product(r1, (Sum(_) as r2)) ->
            (string_of_regexp r1) ^"(" ^ (string_of_regexp r2) ^")"
    | Product(r1, r2) ->
            (string_of_regexp r1) ^ (string_of_regexp r2)
    | Neg(Zero as r) | Neg(One as r) | Neg(Symb(_) as r) -> "~" ^ (string_of_regexp r)
    | Neg(r) -> "~(" ^ (string_of_regexp r) ^ ")"
    | Var(s) -> "[" ^ s ^ "]"


(* main printing function *)
let rec print_regexp (r:regexp) : unit =
    print_string (string_of_regexp r)


(* print a context free language *)
let print_language (l:language) : unit =
    List.iter
        (function x,r ->
            print_string x;
            print_string " -> ";
            print_regexp r;
            print_newline())
        l

(* print the raw regexp, with parenthesis everywhere *)
let rec print_raw_regexp (r:regexp) : unit = match r with
    | Zero -> print_string "0"
    | One -> print_string "1"
    | Symb(a) -> print_char a
    | Star(Zero as r) | Star(One as r) | Star(Symb(_) as r) ->
            print_raw_regexp r; print_string "*"
    | Star(r) -> print_string "("; print_raw_regexp r; print_string ")*"
    | Sum(r1, r2) ->
            print_string "(" ; print_raw_regexp r1; print_string " + ";
            print_raw_regexp r2 ; print_string ")"
    | Product(r1, r2) ->
            print_string "(" ; print_raw_regexp r1; print_string "." ;
            print_raw_regexp r2 ; print_string ")"
    | Neg(r) -> print_string "~(" ; (print_raw_regexp r) ; print_string ")"
    | Var(s) -> print_string s


(***
 *** simplifying a regexp
 ***)

(* get all top-level summands from a regexp *)
let rec get_summands (r:regexp): regexp list = match r with
    | Sum(r1, r2) -> List.rev_append (get_summands r1) (get_summands r2)
          (* addition is commutative, so that the order is unimportant *)
    | Zero -> []
    | r -> [r]

(* its converse: convert a list into a sum *)
let rec list2sum (l:regexp list) :regexp = match l with
    | [] -> Zero
    | [r] -> r
    | r::l -> Sum(r, list2sum l)

(* get all top-level factors from a regexp *)
let rec get_factors (r:regexp): regexp list = match r with
    | Product(r1, r2) -> List.append (get_factors r1) (get_factors r2)
    | r -> [r]

(* its converse: convert a list into a product *)
let rec list2product (l:regexp list) : regexp = match l with
    | [] -> One
    | [r] -> r
    | r::l -> Product(r, list2product l)

(* simplify a toplevel sum, without recursion *)
let simplify_product (r1:regexp) (r2:regexp) : regexp =
    let l = get_factors (Product(r1,r2)) in
    let l = List.filter (fun x -> x <> One) l in
    if List.mem Zero l
    then Zero
    else list2product l

(* simplify a toplevel product, without recursion *)
let simplify_sum (r1:regexp) (r2:regexp) : regexp =
    let l = get_summands (Sum(r1,r2)) in
    let l = List.filter (fun x -> x <> Zero) l in
    let l = List.sort compare l in
    let l = uniq l in
    if List.mem (Neg(Zero)) l
    then Neg(Zero)
    else list2sum l

(* simplify a regexp recursively *)
let rec simplify (r:regexp) : regexp = match r with
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
let contains_epsilon_language (l:language) (r:regexp) : bool =
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

let contains_epsilon (r:regexp) : bool =
    contains_epsilon_language [] r

(* the "derivative with respect to a symbol" of a regular expression *)
module MP = Map.Make(struct type t=regexp let compare = compare end)
module MS = Map.Make(struct type t=string let compare = String.compare end)
module SS = Set.Make(struct type t=string let compare = String.compare end)
let derivative_new (l:language) (x:var) (a:symbol) : language =
    let initial_language = List.fold_left (fun l xr -> MS.add (fst xr) (snd xr) l) MS.empty l in
    let derived_language = ref [] in
    let to_derive = ref (SS.singleton x) in

    let rec get_var r = match r with
        | Zero | One | Symb(_) -> ()
        | Sum(r1,r2) | Product(r1,r2) -> get_var r1; get_var r2
        | Star(r) | Neg(r) -> get_var r
        | Var(x) ->
                if MS.mem x initial_language
                then derived_language := (x, MS.find x initial_language)::!derived_language
    in

    let mem = ref MP.empty in
    let rec derivative_mem (r:regexp) (a:symbol) : regexp =
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
                | Neg(r) -> Neg (derivative_mem r a)
                | Var(x) ->
                        begin
                            let xa = x ^ (String.make 1 a) in
                            if not (List.mem_assoc xa !derived_language)
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
            derived_language := (xa,d)::!derived_language

        done;
        assert false
    with Exit ->
        List.rev !derived_language


(* the "derivative with respect to a symbol" of a regular expression *)
(* module MP = Map.Make(struct type t=regexp let compare = compare end) *)
let derivative ?(l=[]) (r:regexp) (a:symbol) : regexp =
    (* we'll memoize the results *)
    let mem = ref MP.empty in
    let rec derivative_mem (r:regexp) (a:symbol) : regexp =
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
                | Neg(r) -> Neg (derivative_mem r a)
                | Var(s) -> Var(s ^ String.make 1 a)
            in
            mem := MP.add r d !mem;
            d
    in
    derivative_mem r a

let language_derivative (l:language) (a:symbol) : language =
    let rec aux ll acc = match ll with
        | [] -> List.rev_append acc l
        | (x,r)::ll ->
                if List.mem_assoc (x^(String.make 1 a)) l
                then aux ll acc
                else aux ll ((x^(String.make 1 a),derivative ~l:l r a)::acc)
    in
    aux l []

(* the derivative with respect to a word *)
let word_derivative (r:regexp) (w:string) : regexp =
    let rec aux r l = match l with
        | [] -> r
        | a::l -> aux (derivative r a) l
    in
    aux r (explode w)


(* match a string against a regexp using iterated derivatives *)
let match_regexp (w:string) (r:regexp) : bool =
    contains_epsilon (word_derivative r w)


(* the "constant part" of a regexp *)
let constant_part (r:regexp) : regexp =
  if contains_epsilon r
  then One
  else Zero


let language_word_derivative (l:language) (x:var) (w:string) : language =
    let rec aux l x w = match w with
        | [] -> l
        | a::w ->
            let xa = x ^ (String.make 1 a) in
            aux (derivative_new l x a) xa w
    in
    aux l x (explode w)


(* checking if a work belongs to a context-free language using derivatives *)
let match_language (w:string) (l:language) (x:var) : bool =
    let rec aux w l x = match w with
        | [] -> contains_epsilon_language l (Var(x))
        | a::w ->
                let l = derivative_new l x a in
(*                 print_endline "new derivative: "; print_language l; print_newline(); *)
(*                 print_endline ((string_of_int (List.length l)) ^ " equations"); *)
                aux w l (x^(String.make 1 a))
    in
    aux (explode w) l x


(***
 *** misc functions on regexps
 ***)

(* get a list of symbols used in a regexp *)
let get_symbols (r:regexp) : symbol list =
    let rec aux r = match r with
    | One | Zero -> []
    | Symb(a) -> [a]
    | Star(r) -> aux r
    | Sum(r1, r2) | Product(r1, r2) -> List.rev_append (aux r1) (aux r2)
    | Neg(r) -> aux r
    | Var(_) -> assert false
    in
    let l = aux r in
    let l = List.sort compare l in
    let l = uniq l in
    l


(* compute the list of all possible iterated derivatives of a regexp *)
let get_all_derivatives (r:regexp) : (regexp * (symbol list)) list =
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
let rec transpose (r:regexp) : regexp = match r with
    | Zero | One | Symb(_) -> r
    | Sum(r1,r2) -> Sum(transpose r1, transpose r2)
    | Product(r1,r2) -> Product(transpose r2, transpose r1)
    | Star(r) -> Star(transpose r)
    | Neg(r) -> Neg(transpose r)
    | Var(s) -> Var(s ^ "^t")


(* antiderivative *)
let word_antiderivative (r:regexp) (w:string) : regexp =
    let rec aux r l = match l with
        | [] -> transpose r
        | a::l -> aux (derivative r a) l
    in
    let tr = transpose r in
    let l = List.rev (explode w) in
    aux tr l


(* check if a regex denotes the empty language *)
let rec is_empty (r:regexp) : bool = match r with
    | Zero -> true
    | One | Symb(_) -> false
    | Sum(r1,r2) -> is_empty r1 && is_empty r2
    | Product(r1,r2) -> is_empty r1 || is_empty r2
    | Star(_) -> false
    | Neg(r) -> not (is_empty r)
    | Var(_) -> assert false


(* check if the languae of a regular is less than One, ie it contains at most
 * the empty word *)
let rec lessOne (r:regexp) : bool = match r with
    | Zero | One -> true
    | Symb(_) -> false
    | Sum(r1,r2) -> lessOne r1 && lessOne r2
    | Product(r1,r2) -> is_empty r1 || is_empty r2 || (lessOne r1 && lessOne r2)
    | Star(r) -> lessOne r
    | Neg(r) -> raise (Failure "cannot check directly if a complement regexp is less than One")
    | Var(_) -> assert false


(* check if the language of a regexp is infinite *)
let rec is_infinite (r:regexp) : bool = match r with
    | Zero | One | Symb(_) -> false
    | Sum(r1,r2) -> is_infinite r1 || is_infinite r2
    | Product(r1,r2) -> (is_infinite r1 && not (is_empty r2)) ||
                        (not (is_empty r1) && is_infinite r2)
    | Star(r) -> not (lessOne r)
    | Neg(r) -> not (is_infinite r)
    | Var(_) -> assert false


(* compute the regexp of prefixes *)
let rec prefix (r:regexp) : regexp = match r with
    | Zero -> Zero
    | One -> One
    | Symb(a) -> Sum(One,Symb(a))
    | Sum(r1,r2) -> Sum(prefix r1, prefix r2)
    | Product(r1,r2) ->
            let p = simplify_product r1 (prefix r2) in
            simplify_sum (prefix r1) p
    | Star(r) -> simplify_product (Star(r)) (prefix r)
    | Neg(r) -> raise (Failure "cannot compute directly the prefix of a complement regexp")
    | Var(_) -> assert false


(* random regexp of (at most) given depth using a given alphabet *)
let rec random_regexp ?(alphabet=['a';'b';'c';'d';'e']) n =
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
                | x when x < 17 -> Sum(random_regexp (n-1), random_regexp (n-1))
                | x when x < 25 -> Product(random_regexp (n-1), random_regexp (n-1))
                | x when x < 33 -> Star(random_regexp (n-1))
                | _ -> assert false
        end

