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

(* get index of an element in a list *)
let idx x l =
    let rec aux l acc = match l with
    | [] -> raise Not_found
    | y::_ when x=y -> acc
    | _::l -> aux l (acc+1)
    in
    aux l 1


(* print a given number of characters *)
let rec print_n_char c n = if n<=0 then () else (print_char c; print_n_char c (n-1))

(* print a string with padding of spaces of given width *)
let print_string_w s w =
    print_string s;
    print_n_char ' ' (w-String.length s)

let print_char_w a w =
    print_char a;
    print_n_char ' ' (w-1)


(* xor function: exactly one of a and b is true *)
let xor a b = (a && not b) || (not a && b)

