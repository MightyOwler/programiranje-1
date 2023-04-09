let option_sum x y = match x, y  with
| Some a, Some b -> Some (a + b)
| _ -> None

let twostep_map f g h el =
  let x, y = f el
in
  (g x, h y)

let function_repeat f list =
  let rec aux acc k lst = match k, lst with
  | _, [] -> List.rev acc
  | k, [x] -> if k <= 0 then List.rev acc else aux (x :: acc) (k - 1) [x]
  | k, x :: y :: xs -> if k <= 0 then aux acc (f y) (y :: xs) else aux (x :: acc) (k - 1) (x :: y :: xs)
in
aux [] (list |> List.hd |> f) list

let id x = x 

let rec iterate f pogoj = function
  | k when pogoj k -> k
  | k -> iterate f pogoj (f k)



let vecje x = x > 10
let g x = x + 1