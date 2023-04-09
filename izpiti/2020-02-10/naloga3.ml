(* Glede na to, kje smo, bo različno število ustreznih naslednikov*)
let stevilo_naslednikov clen k = k + 1 + min clen k

let ustrezni_range clen k= 
  let range n k =
    let rec aux acc n k = if n = k then (k :: acc) else aux (k :: acc) n (k - 1)
  in
  aux [] n k
  in
  range (clen - k) (clen + k) |> List.filter (fun x -> x >= 0)

(* To nam da seznam, po katerem lahko iteriramo*)

let sum list = List.fold_left (fun acc x -> acc + x) 0 list

let f k n = 
  let rec aux clen = function
 | 0 -> 1
 | 1 -> stevilo_naslednikov clen k
 | l ->  sum (List.map (fun x -> aux x (l-1)) (ustrezni_range clen k))
in
aux 0 (n - 1)