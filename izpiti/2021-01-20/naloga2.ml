(*============================================================================*]
  Filip potrebuje pomoč pri organiziranju kuhinje. Posode in omare mu je uspelo
  popisati in ustrezno označiti, sedaj pa mora nad tem izvajati kopico
  arhivskih nalog, kjer nastopite vi.
[*============================================================================*)

type 'a vsebina_kuhinje =
  | Ponev of 'a
  | Lonec of 'a * 'a
  | Omara of 'a list

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte primer seznama kuhinjskih elementov [kuhinja], kjer ponev vsebuje
  niz "tuna", lonec vsebuje "brokoli" in "mango", omara pa vsebuje "sir",
  "toast", "sok" in "ragu".
[*----------------------------------------------------------------------------*)

(* Primer vsotnega tipa *)
let kuhinja = 
  [Ponev("tuna");
  Lonec(("brokoli", "mango"));
  Omara(["sir";
  "toast"; "sok"; "ragu"])
  ]

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [prestej], ki za podani seznam kuhinjskih elementov vrne
  skupno število vsebinskih elementov. Za zgornji primer [kuhinja] bi tako
  vrnila 7.
[*----------------------------------------------------------------------------*)

let prestej kuhinja =
  let stevilo = function
  | Ponev _ -> 1
  | Lonec _ -> 2
  | Omara lst -> List.length lst
in
List.fold_left (fun acc x -> acc + stevilo x) 0 kuhinja


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme funkcijo [f] in kuhinjski element ter
  funkcijo [f] uporabi na celotni vsebini elementa.

    pretvori : (’a -> ’b) -> ’a kuhinjski_element -> ‘b kuhinjski_element

[*----------------------------------------------------------------------------*)

let rec pretvori f = function
  | Ponev x -> Ponev (f x)
  | Lonec (x, y) -> Lonec (f x, f y)
  | Omara lst -> Omara (List.map f lst)

(* d *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo [pospravi], ki sprejme seznam kuhinjskih elementov in
  vsebino vseh elementov pospravi v eno samo [Omaro]. Vrstni red elementov v
  končni omari je nepomemben. Za vse točke naj bo funkcija repno rekurzivna. 

    pospravi : ’a kuhinjski_element list -> ‘a kuhinjski_element

[*----------------------------------------------------------------------------*)

let pospravi kuhinja =
  let pridobi_iz_elementa = function
  | Ponev x-> [x]
  | Lonec (x, y) -> [x; y]
  | Omara lst -> lst
in
  Omara (List.fold_left (fun acc x -> acc @ pridobi_iz_elementa x) [] kuhinja)

(* e *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [oceni], ki sprejme seznam tipa ['a kuhinjski_element list]
  in cenilko vsebine tipa [‘a -> int]. Funkcija izračuna skupno ceno celotnega
  seznama, kjer je cena vsebine v loncih množena s 3, v omarah pa s 5.

  Ocena testne kuhinje za cenilko [String.length] je 115.
[*----------------------------------------------------------------------------*)

let rec oceni cenilka kuhinja =
  let pomnozi_ceno = function
    | Ponev x -> x
    | Lonec (x, y) -> 3 * (x + y)
    | Omara xs -> 5 * (List.fold_left (+) 0 xs)
  in
  kuhinja |> pretvori cenilka |> List.map pomnozi_ceno |> List.fold_left (+) 0
