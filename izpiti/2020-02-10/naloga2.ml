type ('a, 'b) tree =
| Empty
| ANode of ('a, 'b) tree * 'a * ('a, 'b) tree
| BNode of ('a, 'b) tree * 'b * ('a, 'b) tree

let a_leaf a = ANode(Empty, a, Empty)
let b_leaf b = BNode(Empty, b, Empty)

let primer = ANode(b_leaf true, 12, ANode(a_leaf 0, 5, b_leaf false))

let rec depth tree : int =
  match tree with
  | Empty -> 0
  | ANode (lt, _, rt) -> 1 + max (depth lt) (depth rt)
  | BNode (lt, _, rt) -> 1 + max (depth lt) (depth rt)

let rec adepth = function
| Empty -> 0
| ANode(Empty, _, Empty) -> 1
| BNode(Empty, _, Empty) -> 0
| ANode (l, _, d) -> 1 + max (adepth l) (adepth d)
| BNode (l, _, d) -> 1 + max (adepth l) (adepth d)

let rec bdepth = function
| Empty -> 0
| ANode(Empty, _, Empty) -> 0
| BNode(Empty, _, Empty) -> 1
| ANode (l, _, d) -> 1 + max (adepth l) (adepth d)
| BNode (l, _, d) -> 1 + max (adepth l) (adepth d)

type result = {
  aNodes : int;
  bNodes : int;
}

let rec count_a = function
| Empty -> 0
| ANode(l, _, d) -> 1 + count_a l + count_a d
| BNode(l, _, d) -> count_a l + count_a d

let rec count_b = function
| Empty -> 0
| ANode(l, _, d) -> count_a l + count_a d
| BNode(l, _, d) -> 1 + count_a l + count_a d

let count tree = {aNodes = count_a tree; bNodes = count_b tree}


 
let is_typemirror tree_a tree_b =
  let rec mirror = function
 | Empty -> Empty
 | ANode (l, x, d) -> BNode(mirror l, x, mirror d)
| BNode (l, x, d) -> ANode(mirror l, x, mirror d)
in
tree_b = mirror tree_a
