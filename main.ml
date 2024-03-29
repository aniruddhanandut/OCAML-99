(* Problem 1 *)
let rec last (list: 'a list): 'a option =
    match list with
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest;;

(* Problem 2 *)
let rec last_2 (list: 'a list): ('a * 'a) option = 
    match list with
    | [] | [_] -> None
    | [x;y] -> Some (x,y)
    | _ :: rest -> last_2 rest;;

(* Problem 3 *)
let rec at (k: int) (list: 'a list): 'a option = 
    match list with
    | [] -> None
    | x :: _ when k < 1 -> None 
    | x :: _ when k == 1 -> Some x
    | _ :: rest -> at (k-1) (rest);;

(* Problem 4*)
let length (list: 'a list): (int) = 
    let rec length_helper (k: int) (list: 'a list): 'a = 
        match list with
        | [] -> k
        | _ :: rest -> length_helper (k +1) (rest) in
    length_helper (0) (list);;


(* Problem 5 *)
let rev (list: 'a list): 'a list = 
    let rec rev_helper (prev_list: 'a list) (new_list: 'a list): 'a list = 
        match prev_list with
        | [] -> new_list
        | x :: rest -> rev_helper (rest) (x :: new_list) in
    rev_helper (list) ([]);;

(* Problem 6 *)
let is_palindrome (list: 'a list): bool = 
    list = rev (list);;

(* Problem 7 *)
(*
   1. needs to use tailcall recursive optimization
   *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;
let flatten (nested_list: 'a node list): 'a list = 
    let rec flat_helper (nested_list: 'a node list) (list: 'a list): 'a list = 
        match nested_list with
        | [] -> list
        | One x :: rest -> flat_helper (rest) (list @ [x])
        | Many x :: rest -> flat_helper (rest) (list @ flat_helper (x) ([])) in
    flat_helper (nested_list) ([]);;

(* Problem 8 *)
let rec compress (list: 'a list): 'a list =
    match list with
    | [] -> []
    | [x] -> [x];
    | x :: y :: rest when x <> y -> [x] @ compress (y::rest)
    | x :: y :: rest when x = y -> compress (x::rest);;

(* Problem 9 *)
(*
    Couldn't figure this out :(
        *)
(* Problem 10 *)
let encode (list: 'b list): ('a * 'b) list = 
    let rec encode_helper (list: 'b list) (count: 'a) : ('a * 'b) list = 
        match list with
        | [] -> []
        | [x] -> [(count, x)]
        | x :: y :: rest when x <> y -> [(count, x)] @ encode_helper (y :: rest) (1)
        | x :: y :: rest when x = y -> encode_helper (x :: rest) (count + 1) in
    encode_helper (list) (1);;

(* Problem 11 *)
(*
    There isnt much to do here
    *)


(* Problem 12 *)
(*
    There isnt much to do here
    *)


(* Problem 13 *)
(*
    Boring
    *)
(* Problem 14 *) 
(*
    This was double but i did 15 instead
    *)
(* Problem 15 *)
let rec replicate (list: 'a list) (count: int) : 'a list =
    let rec replicate_helper (value: 'a) (count: int): 'a list = 
        if count = 0 then []
        else if count = 1 then [value]
        else [value] @ replicate_helper (value) (count -1) in
    match list with
    | [] -> []
    | [x] -> replicate_helper (x) (count)
    | x :: rest -> replicate_helper (x) (count) @ replicate (rest) (count);;

(* Problem 16 *)
let drop (list: 'a list) (k: int): 'a list =
    let rec drop_helper (list: 'a list) (k: int) (count: int) : 'a list = 
        match list with
        | [] -> []
        | [x] when count = 1 -> []
        | [x] -> [x]
        | x :: rest when count = 1 -> drop_helper (rest) (k) (k)
        | x :: rest -> x :: drop_helper (rest) (k) (count -1) in
    drop_helper (list) (k) (k);;
(* Problem 17 *)
let split (list: 'a list) (k: int): ('a list * 'a list) = 
    let rec split_helper (list: 'a list) (prev_list: 'a list) (k: int): ('a list * 'a list) =
        match list with
        | [] -> (prev_list, [])
        | x :: rest when k = 0 -> (prev_list, x::rest)
        | x :: rest -> split_helper (rest) (prev_list @ [x]) (k -1) in
    split_helper (list) ([]) (k);;

(* Problem 18 *)
let slice (list: 'a list) (n1: int) (n2: int): ('a list) =
    let rec slice_helper (list: 'a list) (n1: int) (n2: int) (cur: int) : ('a list) = 
        match list with
        | [] -> []
        | x :: rest when cur > n2 -> []
        | x :: rest when cur >= n1 -> x :: slice_helper (rest) (n1) (n2) (cur +1)
        | x :: rest -> slice_helper (rest) (n1) (n2) (cur + 1) in
    slice_helper (list) (n1) (n2) (0);;

(* Problem 19 *)
let rotate (list: 'a list) (k: int): ('a list) =
    let rec rotate_helper (rem_list: 'a list) (cur_list: 'a list) (k: int) : ('a list) = 
        match rem_list with
        | [] -> []
        | x :: rest when k = 0 -> (x :: rest) @ cur_list
        | x :: rest -> rotate_helper (rest) (cur_list @ [x]) (k - 1) in
    let k2 = k mod List.length (list) in
    if k2 > 0 then rotate_helper (list) ([]) (k2 mod List.length (list))
    else if k2 < 0 then rotate_helper (list) ([]) (List.length (list) + k2)
    else list

(*
    Skipping to the arithmathic problems, might come back to lists later
*)

(* Problem 31 *)
let is_prime (k: int) : bool = 
    if k < 2 then false
    else let rec generate_list (k: int) (cur: int) : (int * bool) list = 
        if cur <= k then (cur, false) :: generate_list (k) (cur + 1)
        else [] in
    let rec set_vals (list: (int * bool) list) (value: int) : (int * bool) list = 
        match list with
        | [] -> []
        | (x, _) :: rest when x mod value = 0 -> (x, true) :: set_vals (rest) (value)
        | (x, y) :: rest -> (x, y) :: set_vals (rest) (value) in
    let rec traverse (list: (int * bool) list) (k: int) : bool = 
        match list with
        | [] -> true
        | (_, y) :: rest when y -> traverse (rest) (k)
        | (x, _) :: rest when k mod x = 0 -> false
        | (x, _) :: rest -> traverse (set_vals (rest) (x)) (k) in
    let list = generate_list (int_of_float (sqrt (float_of_int k))) (2) in
    traverse (list) (k);;
(* Problem 32 *)
let rec gcd (k1: int) (k2: int) : (int) = 
    let rem = k1 mod k2 in
    if rem = 0 then k2
    else gcd (k2) (rem);;

(* Problem 33 *)
let coprime (k1: int) (k2: int): bool = 
    gcd (k1) (k2) = 1;;

(* 
   Problem 34 - 41 are also kinda boring
*)

(* Skipping Logic too :( *)
(* Revisiting Logic because I am addicted to ocaml *)
type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;
(* Problem 46 *)
(* This assumes two variables are passed and whats to evaluate the expression*)
(* Because the lists are constant doesnt matter whether its a constant list or using @ to concatnate *)
let table2 (var1: string) (var2: string) (expr: bool_expr): (bool * bool * bool) list = 
    let rec traverse (var1: string) (var2: string) (var1_val: bool) (var2_val: bool) 
    (expr: bool_expr): (bool) = 
        match expr with
        | Var x when x = var1 -> var1_val
        | Var x when x = var2 -> var2_val
        | Not x -> not (traverse (var1) (var2) (var1_val) (var2_val) (x))
        | And (x, y) -> traverse (var1) (var2) (var1_val) (var2_val) (x) && 
            traverse (var1) (var2) (var1_val) (var2_val) (y)
        | Or (x, y) -> traverse (var1) (var2) (var1_val) (var2_val) (x) || 
            traverse (var1) (var2) (var1_val) (var2_val) (y) in
    [(true, true, traverse (var1) (var2) (true) (true) (expr)); 
    (true, false, traverse (var1) (var2) (true) (false) (expr)); 
    (false, true, traverse (var1) (var2) (false) (true) (expr));  
    (false, false, traverse (var1) (var2) (false) (false) (expr))];;
(* Problem 49 *)
(* TODO: This does not work as intended *)
let gray (count: int) : string list =
    let rec generate_string (str: string) (count: int) : string list =
        match count with
        | 0 -> [str]
        | x -> generate_string (str ^ "0") (count -1) @ generate_string(str ^ "1") (count -1) in
    generate_string ("") (count);;

(* Problem 55 *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let cbal_tree (size: int) : char binary_tree list = 
    let rec create_tree (tree: char binary_tree) (size: int) (right: int) (left: int) 
    : char binary_tree list = 
        if right + left = size then [tree]
        else if left > right then create_tree (Node ('x', Empty, tree)) (size) (right+1) (left)
        else create_tree (Node ('x', tree, Empty)) (size) (right) (left+1) in
    create_tree (Empty) (size) (0) (0);;

(*
[Node ('x', Empty, 
    Node ('x', 
        Node ('x', Empty, 
            Node ('x', Empty, Empty)
        ), 
    Empty)
)]
*)

(* Problem 57 *)
(* I was confused on the accepting node in the switch syntax, but I got it now *)
let construct (list: int list) : int binary_tree =
    let rec insert (tree: int binary_tree) (value: int): (int binary_tree) =
        match tree with
        | Empty -> Node (value, Empty, Empty)
        | Node (x, y, z) when x > value -> Node (x, insert (y) (value), z)
        | Node (x, y, z) -> Node (x, y, insert (z) (value)) in
    let rec insert_total (tree: int binary_tree) (list: int list): int binary_tree = 
        match list with
        | [] -> tree
        | x :: rest -> insert_total (insert (tree) (x)) (rest) in
    insert_total (Empty) (list);;

(* Problem 57 part 2 *)
(* I have no idea if I wrote smth valid or if I am on complete crack *)
let rec is_symmetric (tree: 'a binary_tree) : bool =
    let rec size (internal_tree: 'a binary_tree) : int =
        match internal_tree with
        | Empty -> 0
        | Node (x, y, z) -> 1 + size (y) + size (z) in
    match tree with
    | Empty -> true
    | Node(x, y, z) -> size (y) = size(z) && is_symmetric (y) && is_symmetric (z);;











