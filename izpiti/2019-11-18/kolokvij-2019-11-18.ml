(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root a b = a >= 0 && a * a = b

let pack3 a b c = (a,b,c)

let sum list = List.fold_left (fun acc x -> acc + x) 0 list 
let sum_if_not pogoj list = 
  sum (List.filter (fun x -> not (pogoj x)) list)


let apply funkcije elementi =
  let rec aux acc = function
  | [] -> List.rev acc
  | e :: es -> aux ((List.map (fun f -> f e) funkcije) :: acc) es
in
aux [] elementi
  
(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanje | Vaje

type srecanje = {predmet: string; vrsta_srecanja: vrsta_srecanja; trajanje: int}

type urnik = (srecanje list) list

let vaje = {predmet = "Prog 1"; vrsta_srecanja = Vaje; trajanje = 2}
let predavanja = {predmet = "Analiza 2a"; vrsta_srecanja = Predavanje; trajanje = 3}

let urnik_profesor = [[{predmet = "Prog 1"; vrsta_srecanja = Vaje; trajanje = 2}]; []; [{predmet = "Prog 1"; vrsta_srecanja = Predavanje; trajanje = 1}]; []; []; [{predmet = "Prog 1"; vrsta_srecanja = Vaje; trajanje = 1}]; []]

let vec_kot_4_vaje dan = 
  let rec aux count = function
  | [] -> count
  | x :: xs -> match x with
  | {predmet = _; vrsta_srecanja = Vaje; trajanje = x} -> aux (x + count) xs
  | _ -> aux count xs
in
(aux 0 dan) > 3

let vec_kot_4_predavanja dan = 
  let rec aux count = function
  | [] -> count
  | x :: xs -> match x with
  | {predmet = _; vrsta_srecanja = Predavanje; trajanje = x} -> aux (x + count) xs
  | _ -> aux count xs
in
(aux 0 dan) > 3


let rec je_preobremenjen (urnik : urnik)= match urnik with
  | [] -> true
  | x :: xs -> vec_kot_4_vaje x &&  vec_kot_4_predavanja x && je_preobremenjen xs


let izkupicek_dneva dan = 
  let rec aux count = function
  | [] -> count
  | x :: xs -> match x with
    | {predmet = _; vrsta_srecanja = Predavanje; trajanje = x} -> aux (2 * x + count) xs
    | {predmet = _; vrsta_srecanja = Vaje; trajanje = x} -> aux (x + count) xs
in 
aux 0 dan

let rec bogastvo = function
 | [] -> 0
 | x :: xs -> bogastvo xs + izkupicek_dneva x
