type 'a list_tree = Leaf of 'a | Node of 'a list_tree list

(* 1. a) *)

let rec map f = function
  | Leaf(x) -> Leaf(f x)
  | Node [] -> Node []
  | Node(x :: xs) -> Node((map f x) :: (List.map (map f) xs))

(* 1. b) *)

let sum list = List.fold_left (fun acc x -> x + acc) 0 list


let count tree =
  let rec aux acc = function
  | Leaf(_) -> 1 + acc
  | Node [] -> 0
  | Node(x :: xs) ->  aux acc x + sum (List.map (aux acc) xs)
in
aux 0 tree

(* 1. c) *)

let rec apply fun_tree tree = match fun_tree, tree with
| Leaf (f), Leaf(a) -> Leaf(f a)
| Node [], Node [] -> Node [] 
| Node(x :: xs), Node(y :: ys) -> Node((apply x y) :: (List.map2 apply xs ys))
| _ -> failwith "Drevesi nista iste oblike, ne goljufaj"

(* 1. d) *)
let compose f g = fun x -> f (g x)
let rec combine fun_tree gun_tree = match fun_tree, gun_tree with
| Leaf (f), Leaf(g) -> Leaf(compose f g)
| Node [], Node [] -> Node [] 
| Node(x :: xs), Node(y :: ys) -> Node((combine x y) :: (List.map2 combine xs ys))
| _ -> failwith "Drevesi nista iste oblike, ne goljufaj"


(* 1. e) *)

(* Tukaj bi se morda dalo uporabiti apply, tako da bi dopolnili kako drevo do drugega z default vrednostmi*)



let t1 = Node [ Node [ Leaf (fun x -> x) ]; Leaf (fun x -> x * 2) ]
let t2 = Node [ Leaf 1; Leaf 2 ]
let t3 = Node [ Node []; Leaf 2; Leaf 4 ]
