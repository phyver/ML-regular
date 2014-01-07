(***
 *** utility functions
 ***)

(* remove duplicates from a sorted list *)
let rec uniq l =
    let rec aux l acc = match l with
        | [] -> List.rev acc
        | [r] -> List.rev (r::acc)
        | r1::r2::l when r1=r2 -> aux (r2::l) acc
        | r1::r2::l -> aux (r2::l) (r1::acc)
    in aux l []

(* transform a string into a list of characters *)
let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l)
    in
        exp (String.length s - 1) []


(***
 *** regular expressions and their derivatives
 ***)

(* type for symbols *)
type alphabet = char


(* type for basic regular expressions *)
type regexp =
  | Zero
  | One
  | Symb of alphabet
  | Sum of regexp*regexp
  | Product of regexp*regexp
  | Star of regexp


(* print a regexp *)
let rec print_regexp (r:regexp) : unit =
    match r with
    | Zero -> print_string "0"
    | One -> print_string "1"
    | Symb(a) -> print_char a
    | Star(Zero as r) | Star(One as r) | Star(Symb(_) as r) -> print_regexp r; print_string "*"
    | Star(r) -> print_string "("; print_regexp r; print_string ")*"
    | Sum(r1, r2) -> print_regexp r1; print_string " + "; print_regexp r2
    | Product(r1, r2) -> print_regexp r1; print_regexp r2

(* print the raw regexp, with parenthesis everywhere *)
let rec print_raw_regexp (r:regexp) : unit =
    match r with
    | Zero -> print_string "0"
    | One -> print_string "1"
    | Symb(a) -> print_char a
    | Star(Zero as r) | Star(One as r) | Star(Symb(_) as r) -> print_raw_regexp r; print_string "*"
    | Star(r) -> print_string "("; print_raw_regexp r; print_string ")*"
    | Sum(r1, r2) -> print_string "(" ; print_raw_regexp r1; print_string " + "; print_raw_regexp r2 ; print_string ")"
    | Product(r1, r2) -> print_string "(" ; print_raw_regexp r1; print_string "." ; print_raw_regexp r2 ; print_string ")"


(* get all top-level summands from a regexp *)
let rec get_summands (r:regexp): regexp list =
  match r with
  | Sum(r1, r2) -> List.rev_append (get_summands r1) (get_summands r2)
        (* addition is commutative, so that the order is unimportant *)
  | r -> [r]

(* its converse: convert a list into a sum *)
let rec list2sum l = match l with
  | [] -> Zero
  | [r] -> r
  | r::l -> Sum(r, list2sum l)

(* get all top-level factors from a regexp *)
let rec get_factors (r:regexp): regexp list =
  match r with
  | Product(r1, r2) -> List.append (get_factors r1) (get_factors r2)
  | r -> [r]

(* its converse: convert a list into a product *)
let rec list2product l = match l with
  | [] -> One
  | [r] -> r
  | r::l -> Product(r, list2product l)


(* simplify a regexp *)
let rec simplify (r:regexp) : regexp = match r with
  | One | Zero | Symb(_) -> r
  | Star(r) ->
          begin
              let r = simplify r in
              match r with 
                | Zero -> Zero
                | One -> One
                | Star(r) -> Star(r)
                | _ -> Star(simplify r)
          end
  | Product(r1, r2) ->
          let r1 = simplify r1 in
          let r2 = simplify r2 in
          let l = get_factors (Product(r1,r2)) in
          let l = List.filter (fun x -> x <> One) l in
          if List.mem Zero l
          then Zero
          else list2product l
  | Sum(r1, r2) ->
          let r1 = simplify r1 in
          let r2 = simplify r2 in
          let l = get_summands (Sum(r1,r2)) in
          let l = List.filter (fun x -> x <> Zero) l in
          let l = List.sort compare l in
          let l = uniq l in
          list2sum l


(* check if the empty string is matched by a regexp *)
let rec contains_epsilon (r:regexp) : bool =
    match r with
    | One -> true
    | Zero -> false
    | Symb(_) -> false
    | Sum(r1, r2) -> contains_epsilon r1 || contains_epsilon r2
    | Product(r1, r2) -> contains_epsilon r1 && contains_epsilon r2
    | Star r -> true


(* the "constant part" of a regexp *)
let constant_part (r:regexp) : regexp =
  if contains_epsilon r
  then One
  else Zero


(* the "derivative with respect to a symbol" of a regular expression *)
let rec derivative (r:regexp) (a:alphabet) : regexp =
  match r with
  | Zero | One -> Zero
  | Symb(b) when b = a -> One
  | Symb(_) -> Zero
  | Sum(r1, r2) -> Sum(derivative r1 a, derivative r2 a)
  | Product(r1, r2) -> Sum(Product(derivative r1 a, r2), Product(constant_part r1, derivative r2 a))
  | Star(r) -> Product (derivative r a, Star(r))


(* the word derivative *)
let word_derivative (r:regexp) (s:string) : regexp =
    let rec aux r l = 
        match l with
        | [] -> constant_part r
        | a::l -> aux (derivative r a) l
    in
    aux r (explode s)


(* match a string against a regexp using iterated derivatives *)
let match_regexp (s:string) (r:regexp) : bool =
    One = word_derivative r s


(* get a list of symbols used in a regexp *)
let get_symbols (r:regexp) : alphabet list =
    let rec aux r = match r with
    | One | Zero -> []
    | Symb(a) -> [a]
    | Star(r) -> aux r
    | Sum(r1, r2) | Product(r1, r2) -> List.rev_append (aux r1) (aux r2)
    in
    let l = aux r in
    let l = List.sort compare l in
    let l = uniq l in
    l


(* compute the list of all possible iterated derivatives of a regexp *)
let get_all_derivatives (r:regexp) : regexp list =
    let symbols = get_symbols r in

    let rec union l1 l2 = match l2 with
       | [] -> l1
       | a::l2 -> if List.mem a l1
                  then union l1 l2
                  else union (a::l1) l2
    in

    let rec aux ok todo = match todo with
        | [] -> ok
        | r::l ->
                if List.mem r ok
                then aux ok l
                else
                    let ld = List.map (fun a -> derivative r a) symbols in
                    let ld = List.map simplify ld in
                    let todo = union todo ld in
                    aux (r::ok) todo
   in
   aux [] [simplify r]

