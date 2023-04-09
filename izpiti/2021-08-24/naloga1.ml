(* a *)
(*----------------------------------------------------------------------------*]
  Napišite predikat `je_urejena : int * int * int -> bool`, ki pove, ali je 
  podana trojica celih števil urejena strogo naraščajoče.
[*----------------------------------------------------------------------------*)

let je_urejena trojica = 
  let a, b, c = trojica in 
  if a < b && b < c then true else false

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `poskusi_deljenje : float option -> float option -> float option`, 
  ki sprejme morebitni deljenec in morebitni delitelj ter vrne rezultat deljenja, 
  če se to da, ali pa `None`, če ne (kadar kakšnega argumenta ni ali pa bi prišlo 
  do deljenja z nič). 
  
    # poskusi_deljenje (Some 1.0) (Some 2.0);;
    - : float option = Some 0.5
    # poskusi_deljenje (Some 1.0) (Some 0.0);;
    - : float option = None
    # poskusi_deljenje None (Some 2.0);;
    - : float option = None
[*----------------------------------------------------------------------------*)

let poskusi_deljenje n k = 
  match n, k  with
  | _, Some 0.0 -> None
  | Some n, Some k -> Some(n /. k)
  | _ -> None

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo `zavrti : 'a list -> int -> 'a list`, ki seznam zavrti 
  za dano število mest v levo (v vsaki rotaciji se prvi element prestavi na 
  konec seznama).
  
    # zavrti [1; 2; 3; 4; 5] 2;;
    - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let zavrti list n = 
  let rec aux acc lst = function
  | 0 -> lst @ (List.rev acc)
  | k -> match lst with 
    | x :: xs -> aux (x :: acc) xs (k -1)
    | _ -> assert false
in 
aux [] list (n mod (List.length list))

(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo `razdeli : ('a -> int) -> 'a list -> ('a list *  'a list * 'a list)|`, 
  ki sprejme cenilno funkcijo in seznam elementov. Vrne naj trojico, kjer so na prvem 
  mestu vsi elementi za katere je cenilna funkcija negativna, na drugem vsi, kjer 
  je enaka 0, na tretjem pa vsi preostali elementi.
  Elementi naj v seznamih nastopajo v enakem vrstnem redu kot v prvotnem seznamu. 
  Za vse točke naj bo funkcija repno rekurzivna.
  
    # razdeli ((-) 3) [1; 2; 3; 4; 5; 6];;
    - : int list * int list * int list = ([4; 5; 6], [3], [1; 2])
[*----------------------------------------------------------------------------*)

let razdeli f list = 
  let rec aux acc1 acc2 acc3 = function
  | [] -> (List.rev acc3, List.rev acc2, List.rev acc1)
  | x :: xs -> match f x with
  | 0 -> aux acc1 (x :: acc2) acc3 xs
  | k when k > 0 -> aux (x :: acc1) acc2 acc3 xs
  | _ -> aux acc1 acc2 (x :: acc3) xs
in
aux [] [] [] list