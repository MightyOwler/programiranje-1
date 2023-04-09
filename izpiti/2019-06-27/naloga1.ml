type complex = { re : float ; im : float }

let complex_add a b =
  match a, b with
  | { re = rea ; im = ima }, { re = reb; im = imb } -> {re = rea +. reb; im = ima +. imb}

let complex_conjugate a =
  match a with
  | { re = rea ; im = ima } -> { re = rea ; im = -. ima }


let rec list_apply_either pogoj f g = function
| [] -> []
| x :: xs -> if pogoj x then (f x) :: list_apply_either pogoj f g xs else (g x) :: list_apply_either pogoj f g xs

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let eval_poly koef a = 
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (acc + x * pow a (List.length xs)) xs
in
aux 0 (List.rev koef)