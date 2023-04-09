type 'a improved_list = 
 | Empty
 | ImprovedList of 'a array * 'a improved_list

let seznam = ImprovedList([| 1; 2; 20 |] , ImprovedList([| 17; 19; 20; 30 |], ImprovedList([| 100 |], Empty)))
let seznam2 = ImprovedList([| 1; 2; 20 |] , ImprovedList([| 27; 29; 30; 40 |], ImprovedList([| 100 |], Empty)))


let rec count = function
 | Empty -> 0
 | ImprovedList(a ,b) -> (a |> Array.length) + count b

let rec list_nth list n = match (list, n) with
| [], _ -> None
| _, k when k < 0 -> None
| x :: xs, 0 -> Some x
| x :: xs, k -> list_nth xs (k-1)

let rec nth imp n = 
  if count imp < n + 1 then None
  else
  match imp with
  | ImprovedList(lst, rest) -> if n < Array.length lst then list_nth (Array.to_list lst) n else nth rest (n - (Array.length lst))
  | Empty -> assert false

let is_ordered_array a =
  let len = Array.length a in
  let rec check i =
    if i >= len - 1 then true
    else if a.(i) > a.(i + 1) then false
    else check (i + 1)
  in
  check 0;;


let rec is_ordered imp =
  match imp with
  | Empty -> true
  | ImprovedList(list, rest) -> match list, rest with
    | arr, Empty -> is_ordered_array arr
    | [||], rst -> true && is_ordered rst
    | arr, rst -> 
      match nth rst 0 with
      | Some y -> is_ordered_array arr && ((arr |> Array.to_list |> List.rev |> List.hd) < y) && is_ordered rst
      | None -> assert false

let overwrite a x i =
  let len = Array.length a in
  let res = Array.make len 0 in
  Array.blit a 0 res 0 len;
  res.(i) <- x;
  res
      
    
let rec update imp n value = 
  match imp, n with
  | Empty, _ -> failwith "napačen indeks"
  | ImprovedList(arr, rest), st -> match arr with
    | arr when Array.length arr > st -> ImprovedList (overwrite arr value st,rest)
    | arr -> ImprovedList (arr, update rest (n - Array.length arr) value)


  

 