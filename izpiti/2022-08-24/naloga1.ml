(* 1. a) *)
let zamenjaj a b =
  let (x, y) = a in  
  let (x', y') = b in
  ((x, x'), (y, y'))

(* 1. b) *)
let modus a = let x, y, z = a in
  if x = y && x = z then Some x
  else if x = y && x != z then Some x
  else if y = z && x != z then Some y
  else if z = x && x != y then Some z
  else None
(* 1. c) *)
let uncons = function
 | [] -> None
 | x :: xs -> Some ([x], xs)

(* 1. d) *)
let rec vstavljaj el = function
| [] -> []
| [x] -> [x]
| x :: xs -> x :: el :: vstavljaj el xs

(* 1. e) *)
let popolnoma_obrni list = 
let rec aux_bedrock acc = function
  | [] -> acc
  | x :: xs -> aux_bedrock (x :: acc) xs
in  
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (aux_bedrock [] x :: acc) xs
in 
aux [] list