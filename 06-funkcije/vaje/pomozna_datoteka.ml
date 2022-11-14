let rec reverse = function
| [] -> []
| [x] -> [x]
| x :: xs -> reverse(xs) @ [x]

let map_tlrec f lst= 
  let rec map_tlrec_aux acc = function
  | [] -> reverse acc
  | x :: xs -> map_tlrec_aux (f x :: acc) xs
in map_tlrec_aux [] lst

let mapi f lst = 
  let rec mapi_aux acc n = function
  | [] -> reverse acc
  | x :: xs -> mapi_aux ((f x n) :: acc) (n + 1) xs 
in mapi_aux [] 0 lst

let rec zip sez_A sez_B = match with sez_A, sez_B
  | [], [] -> []
  | [], ys -> failwith "Seznama nista enako dolga"
  | xs, [] -> failwith "Seznama nista enako dolga"
  | x :: xs, y :: ys -> (x, y) :: zip (xs) (ys)