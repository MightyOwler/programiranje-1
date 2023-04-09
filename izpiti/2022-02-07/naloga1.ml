(* 1. a) *)
let je_idempotent c = 
  let (a, b) = c in
  let (x, y) = a in 
  let (x', y') = b in 
  ((x * x + x' * y, x * y + y * y'), (x * x' + x' * y', x' * y + y' * y')) = (a, b)
  
  

(* 1. b) *)
let produkt list =
  let rec aux acc = function
    | [] -> acc
    | 0 :: xs -> aux acc xs
    | x :: xs -> aux (x * acc) xs
in
aux 1 list

(* 1. c) *)

(* let zadnji list = 
  list |> List.rev |> List.hd *)

let stalin_sort list =
  let rec aux acc = function
  | [] -> List.rev acc
  | x :: xs -> if x <= (List.hd acc) then aux acc xs else aux (x :: acc) xs
  in
  aux ([List.hd list]) list


(* 1. d) *)
let splosni_collatz f g pogoj z k = 
  let rec aux acc = function
  | x when x = k -> List.rev acc
  | x when pogoj x -> aux (f x :: acc) (f x)
  | x -> aux (g x :: acc) (g x)
in
aux [z] z
