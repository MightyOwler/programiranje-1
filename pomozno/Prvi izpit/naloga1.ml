(* 1. a) *)

let permutacije (x,y,z) =
  [(x, y, z); (x, z, y); (y, x, z); (y, z, x); (z, x, y); (z, y, x)]

(* 1. b) *)

let rec zip_opt list1 list2 = match list1, list2 with
| [], [] -> []
| [], x :: xs -> (None, Some x) :: (zip_opt [] xs) 
| x::xs, [] -> (Some x, None) :: (zip_opt xs [])
| x:: xs, y :: ys -> (Some x, Some y) :: (zip_opt xs ys)


(* 1. c) *)

let rec zip_default l1 l2 p1 p2 = match l1, l2 with
| [], [] -> []
| [], x :: xs -> (p1, x) :: (zip_default [] xs p1 p2) 
| x::xs, [] -> (x, p2) :: (zip_default xs [] p1 p2)
| x:: xs, y :: ys -> (x, y) :: (zip_default xs ys p1 p2)

(* 1. d) *)

type response = Left | Middle | Right

let distribute f list =
  let rec aux a1 a2 a3 = function
  | [] -> List.map List.rev [a1; a2; a3]
  | x :: xs -> match f x with
  | Left -> aux (x :: a1) a2 a3 xs  
  | Middle -> aux a1 (x :: a2) a3 xs  
  | Right -> aux a1 a2 (x :: a3) xs
in
aux [] [] [] list


(* 1. e) *)

type ('a, 'b) sum = Left of 'a | Right of 'b

(* To bi delalo, če bi znal pravilno odpakirati funkcije*)

let iso1 preslikava = match preslikava with
| (fun x -> match x with | Left(a) -> c | Right(b) -> d) -> ((fun a -> c), (fun b -> d))

let iso2 preslikava = match preslikava with
| ((fun a -> c), (fun b -> d)) ->
  (fun x -> match x with
  | Left(a) -> c
  | Right(b) -> d)