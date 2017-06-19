(***************************************************************)
(*  Copyright 2014 Pierre Hyvernat. All rights reserved.       *)
(*  This file is distributed under the terms of the            *)
(*  GNU General Public License, described in file COPYING.     *)
(***************************************************************)

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
    in
    aux l []

(* compute the intersection of 2 sorted lists *)
let merge_inter (l1:'a list) (l2:'a list) : 'a list =
    let rec aux l1 l2 acc = match l1,l2 with
        | [],l | l,[] -> List.rev acc
        | a1::_, a2::l2 when a1>a2 -> aux l1 l2 acc
        | a1::l1, a2::_ when a1<a2 -> aux l1 l2 acc
        | a1::l1, a2::l2 (* when a1=a2 *) -> aux l1 l2 (a1::acc)
    in
    aux l1 l2 []

(* compute the union of 2 sorted lists *)
let merge_union (l1:'a list) (l2:'a list) : 'a list =
    let rec aux l1 l2 acc = match l1,l2 with
        | [],l | l,[] -> List.rev_append acc l
        | a1::_, a2::l2 when a1>a2 -> aux l1 l2 (a1::acc)
        | a1::l1, a2::_ when a1<a2 -> aux l1 l2 (a2::acc)
        | a1::l1, a2::l2 (* when a1=a2 *) -> aux l1 l2 (a1::acc)
    in
    aux l1 l2 []

(* transform a string into a list of characters *)
let explode s =
    let rec exp i l = if i<0 then l else exp (i-1) (s.[i]::l)
    in
    exp (String.length s - 1) []
(* and converse *)
let implode l =
    let res = Bytes.create (List.length l) in
    let rec imp i = function
        | [] -> res
        | c :: l -> Bytes.set res i c; imp (i + 1) l
    in
    imp 0 l

(* get index of an element in a list *)
let idx x l =
    let rec aux l acc = match l with
        | [] -> raise Not_found
        | y::_ when x=y -> acc
        | _::l -> aux l (acc+1)
    in
    aux l 1

(* print a given number of characters *)
let rec print_n_char c n =
    if n<=0 then () else (print_char c; print_n_char c (n-1))

(* print a string with padding of spaces of given width *)
let print_string_w s w =
    print_string s;
    print_n_char ' ' (w-String.length s)

let print_char_w a w =
    print_char a;
    print_n_char ' ' (w-1)


(* xor function: exactly one of a and b is true *)
let xor a b = (a && not b) || (not a && b)

(* shuffle a list randomly *)
let shuffle l =
    let l = List.map (fun x -> (Random.nativeint Nativeint.max_int, x)) l in
    let l = List.sort compare l in
    List.map snd l


(***
 *** module for ordered types and "to_string" function
 ***)
module type OType = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
end


(***
 *** we will need states to be closed under indexing, pairing and finite sets
 ***)
type 'a generalized_state =
    | Dummy of string
    | Atom of 'a
    | In of int*'a generalized_state
    | Pair of 'a generalized_state * 'a generalized_state
    | FSet of 'a generalized_state list


module GeneralizedState (State:OType)
  : OType with type t=State.t generalized_state
  = struct

    type t = State.t generalized_state

    let rec list_compare cmp l1 l2 = match l1,l2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> +1
        | a1::l1, a2::l2 ->
                let c = cmp a1 a2 in
                if c = 0
                then list_compare cmp l1 l2
                else c

    let rec compare s1 s2 = match s1,s2 with
        | Dummy(s1), Dummy(s2) -> String.compare s1 s2
        | Dummy(_), _ -> -1
        | _, Dummy(_) -> +1
        | Atom(a1), Atom(a2) -> State.compare a1 a2
        | Atom(_), _ -> -1
        | _, Atom(_) -> +1
        | In(n1,s1), In(n2,s2) ->
                if n1<n2 then -1
                else if n1>n2 then +1
                else compare s1 s2
        | In(_), _ -> -1
        | _, In(_) -> +1
        | Pair(s1,s2), Pair(p1,p2) ->
                let c = compare s1 p1 in
                if c = 0
                then compare s2 p2
                else c
        | Pair(_), _ -> -1
        | _, Pair(_) -> +1
        | FSet(l1), FSet(l2) ->
                list_compare compare l1 l2

    let rec to_string = function
        | Dummy(s) -> s
        | Atom(a) -> State.to_string a
        | In(n,s) -> (string_of_int n) ^ ":" ^ (to_string s)
        | Pair(s1,s2) ->
                "<" ^ (to_string s1) ^ " , " ^ (to_string s2) ^ ">"
        | FSet(l) ->
                begin
                    let lstr = List.map to_string l in
                    match lstr with
                    | [] -> "{}"
                    | hd::tl ->
                            let str = List.fold_right
                                        (fun s ss -> " , " ^ s ^ ss)
                                        tl
                                        ""
                            in
                            "{" ^ hd ^ str ^ "}"
                end
end

