(*
  Veliko uporabnih funckij se nahaja tukaj https://ocaml.org/problems
*)


let rec last = function
| [] -> assert false
| x :: [] -> x
| x :: xs -> last xs

let rec last_two = function
| [] | [_] -> None
| [x; y] -> Some (x, y)
| _ :: xs -> last_two xs

let rec list_nth list n = match (list, n) with
| [], _ -> None
| _, k when k < 0 -> None
| x :: xs, 0 -> Some x
| x :: xs, k -> list_nth xs (k-1)

let length list = 
  let rec length_aux acc list = match list with
  | [] -> acc
  | x :: xs -> length_aux (acc + 1) xs
in 
  length_aux 0 list

let reverse list = 
  let rec reverse_aux acc list = match list with
  | [] -> acc
  | x :: xs -> reverse_aux (x :: acc) xs
in 
reverse_aux [] list

let palindrome list = 
  list = reverse list

type 'a node =
| One of 'a 
| Many of 'a node list

let objekt = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

let rec flatten = function
  | [] -> []
  | One x :: xs -> x :: flatten xs
  | Many x :: xs -> (flatten x) @ flatten xs


let compress list = 
  let rec aux acc list = match list with
  | [] -> acc
  | x :: xs -> if List.mem x acc then aux acc xs else aux (x :: acc) xs
in
List.rev (aux [] list)

let pack list = 
  let rec pack_aux acc sub_acc list = match list with
    | [] -> acc
    | [x] -> (x :: sub_acc) :: acc
    | x :: (y :: xs as t) -> 
      if x = y then pack_aux acc (x :: sub_acc) t
      else pack_aux ((x :: sub_acc) :: acc) [] t
    in
  List.rev (pack_aux [] [] list)

(* let encode list = 
  let packed = pack list in
  let rec aux_encode acc list = match list with
  | [] -> List.rev acc
  | x :: xs -> aux_encode ((List.length x, List.hd x):: acc) xs in
  aux_encode [] packed *)

type 'a rle =
| One of 'a
| Many of int * 'a


let encode list = 
  let packed = pack list in
  let rec aux_encode acc list = match list with
  | [] -> List.rev acc
  | x :: xs -> match x with
    | [y] -> aux_encode ((One y) :: acc) xs
    | y :: _ -> aux_encode ((Many (List.length x, y)) :: acc) xs
    | [] -> assert false
     in
  aux_encode [] packed


let decode rle_list =
  let rec aux acc rle_list = match rle_list with
  | [] -> List.rev acc
  | One x :: xs -> aux (x :: acc) xs
  | Many (n, str) :: xs -> match n with
    | 1 -> aux (str :: acc) xs
    | k -> aux (str :: acc) (Many(k-1, str) :: xs)
in
aux [] rle_list


let rec duplicate = function
| [] -> []
| x :: xs -> x :: x :: duplicate xs

let replicate list n = 
  let rec aux acc k list = match k, list with
  | _, [] -> List.rev acc
  | 0, _ -> [] 
  | 1, x :: xs -> aux (x :: acc) n xs
  | k, (x :: xs as t) ->  aux (x :: acc) (k-1) t
in
aux [] n list


let drop list n = 
  let rec aux acc list k = match list, k with
  | [], _ -> List.rev acc
  | x :: xs, 1 -> aux acc xs n
  | x :: xs, k -> aux (x :: acc) xs (k - 1)
in
aux [] list n

let split list n = 
  let rec aux acc lst k = match lst, k with
  | x :: xs, 1 -> [List.rev (x :: acc); xs]
  | [], k -> [list; []]
  | x :: xs, k -> aux (x :: acc) xs (k-1)
in 
aux [] list n

let slice list m n = 
  let rec aux acc lst i k = match lst, i, k with
  | [], _, _ -> List.rev acc
  | x :: xs, k, l-> 
    if k = l then List.rev (x :: acc) 
    else if k < m then 
      aux acc xs (k + 1) l
    else
      aux (x :: acc) xs (k + 1) l
  in
  aux [] list 0 n


let rotate list n =
  let divided = split list (n mod (List.length list)) in 
    match divided with
    | [dx; dy] -> dy @ dx
    | _ -> []

let rec remove_at n lst = match n, lst with
  | 0, x :: xs -> xs
  | k, x :: xs -> x :: (remove_at (k-1) xs)
  | _, [] -> []
  

let rec insert_at element n lst = match n, lst with
  | 1, x :: xs -> x :: element :: xs
  | k, x :: xs -> x :: (insert_at element (k-1) xs)
  | _, [] -> []


let range n k =
  let rec aux acc n k = if n = k then (k :: acc) else aux (k :: acc) n (k - 1)
in
aux [] n k


(* Priredi naključno permutacijo seznama *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let rec extract k list =
  if k <= 0 || k > (list |> List.length) then [[]]
  else match list with
        | [] -> []
        | h :: tl ->
          (* Tiste z glavo rekurzivno dopolnjujemo (prvi člen prilepimo na glavo naslednjih), hkrati pa rekurzivno nadaljujemo na repu in tako dobimo člene brez*)
          let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
          let without_h = extract k tl in
          with_h @ without_h;;


let rec remove_from_list element = function
  | [] -> []
  | x :: xs -> if x = element then remove_from_list element xs else x :: remove_from_list element xs


let rec remove_occurences list to_be_removed_list = match list, to_be_removed_list with
| (_ as t), [] -> t
| [], _ -> []
| (x :: xs as t), y :: ys -> remove_occurences (remove_from_list y t) ys


let rec group list len = 
  List.map (fun l -> [l; remove_occurences list l]) (extract len list)

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1)) in
  n <> 1 && is_not_divisor 2

let rec gcd a b = 
  if b = 0 then a else gcd b (a mod b)

let coprime a b = 
  gcd a b = 1


let phi n = 
  let rec aux acc = function
    | k when k >= n -> acc
    | k -> if coprime k n then aux (acc + 1) (k+1) else aux acc (k+1)
in
aux 0 1

let factors n = 
  let rec aux acc m = function
    | k when k > m -> List.rev acc
    | k -> if coprime k m then aux acc m (k + 1) else aux (k :: acc) (m / k) 2
in
aux [] n 2

let encode list = 
  let packed = pack list in
  let rec aux_encode acc list = match list with
  | [] -> List.rev acc
  | x :: xs -> aux_encode ((List.length x, List.hd x):: acc) xs in
  aux_encode [] packed

let decomposition n = n |> factors |> encode 
(* |> List.map (fun (_, x) -> x) *)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
let euler_totient n = 
  let decomposed = decomposition n in
  let rec aux = function
  | [] -> 1
  | (a, p) :: xs -> (p -1) * (pow p (a - 1)) * aux xs
in
aux decomposed

let timeit f a =
  let t0 = Unix.gettimeofday() in
    ignore (f a);
  let t1 = Unix.gettimeofday() in
    t1 -. t0


let rec gray = function
| k when k < 1 -> []
| 1 -> ["0"; "1"]
| k -> List.map (fun str -> "0" ^ str) (gray (k-1)) @ List.map (fun str -> "1" ^ str) (gray (k-1))

let gray_fold n =
  let rec gray_next_level k l =
    if k < n then
      (* This is the core part of the Gray code construction.
       * first_half is reversed and has a "0" attached to every element.
       * Second part is reversed (it must be reversed for correct gray code).
       * Every element has "1" attached to the front.*)
      let (first_half,second_half) =
        List.fold_left (fun (acc1,acc2) x ->
            (("0" ^ x) :: acc1, ("1" ^ x) :: acc2)) ([], []) l
      in
      (* List.rev_append turns first_half around and attaches it to second_half.
       * The result is the modified first_half in correct order attached to
       * the second_half modified in reversed order.*)
      gray_next_level (k + 1) (List.rev_append first_half second_half)
    else l
  in
    gray_next_level 1 ["0"; "1"];;


(* Spodaj je veliko uporabnih funckij za drevesa!! *)

(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree


(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)
let leaf a = Node (Empty, a, Empty)

let three_subtree = Node (Empty, "g", leaf "t")
let seven_subtree = leaf "a"
let ten_subtree = Node (leaf "e", "r", Node(leaf "t", "z", Empty))

let test_tree : (int, string) lexi_tree = 
  Node (leaf (3, three_subtree), (7, seven_subtree), leaf (10, ten_subtree))

(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)
let rec mem a = function
  | Empty -> false
  | Node (lt, x, rt) when x = a -> true
  | Node (lt, x, rt) when x < a -> mem a rt
  | Node (lt, x, rt) (* x > a *) -> mem a lt

let rec lexi_mem (a, b) = function
  | Empty -> false
  | Node (lt, (x, btree), rt) when x = a -> mem b btree 
  | Node (lt, (x, btree), rt) when x < a -> lexi_mem (a, b) rt 
  | Node (lt, (x, btree), rt) (* x > a *) -> lexi_mem (a, b) lt 


(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)
let rec insert a = function
  | Empty -> leaf a
  | Node (lt, x, rt) when x = a -> Node (lt, x, rt)
  | Node (lt, x, rt) when x < a -> Node (lt, x, insert a rt) 
  | Node (lt, x, rt) (* x < a *) -> Node (insert a lt, x, rt) 

let rec lexi_insert (a, b) = function
  | Empty -> leaf (a, leaf b)
  | Node (lt, (x, btree), rt) when x = a ->
      Node (lt, (x, insert b btree), rt)
  | Node (lt, (x, btree), rt) when x < a -> 
      Node (lt, (x, btree), lexi_insert (a, b) rt)
  | Node (lt, (x, btree), rt) (* x > a *) ->
      Node (lexi_insert (a, b) lt, (x, btree), rt)


(* LEKSIKOGRAFSKA DREVESA *)

(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)
let rec fold f acc = function
  | Empty -> acc
  | Node (lt, x, rt) ->
      let left = fold f acc lt in
      let this = f left x in
      fold f this rt
  
let lexi_fold f acc lexi_tree =
  fold 
    (fun acc (x, tree) -> fold (fun acc -> f acc x) acc tree) 
    acc lexi_tree

(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)
let to_list tree = lexi_fold (fun acc a b -> (a, b) :: acc) [] tree |> List.rev


(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme seznam celih števil in najprej IZPIŠE vsa
  soda števila v seznamu, nato pa IZPIŠE še vsa liha števila v seznamu.
  Števila naj bodo izpisana v isti vrstici in med njimi ne želimo presledkov.

    izpisi_soda_liha : int list -> unit

    # izpisi_soda_liha [3; 1; 4; 1; 5; 9; 2];;
    4231159- : unit = ()
    # izpisi_soda_liha [2; 7; 1; 8; 2; 8; 1];;
    2828711- : unit = ()

[*----------------------------------------------------------------------------*)

let izpisi_soda_liha list =
  list |> List.filter (fun x -> x mod 2 == 0) |> List.iter print_int;
  list |> List.filter (fun x -> x mod 2 == 1) |> List.iter print_int

let izpisi_liha_soda list =
  list |> List.filter (fun x -> x mod 2 = 1) |> List.iter print_int;
  list |> List.filter (fun x -> x mod 2 = 0) |> List.iter print_int


  (* Zapisni tipi*)

type student = {
ime : string;
mutable priimek : string;
vpisna : int;
ocene : int list;
}

type 'a sklic = { mutable vsebina : 'a }

let sklic x = { vsebina = x }

let ( ! ) s = s.vsebina

let ( := ) s x = s.vsebina <- x


(* Napredni seznami *)

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


  
(* Naloge s števcem *)


type magic = Fire | Arcane | Frost
type specialisation = Historian | Teacher | Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status. Zapisni tip je nekaj takega kot slovar.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status = 
| Newbie
| Student of magic * int
| Employed of magic * specialisation

type wizard = {name: string; status: status}

let professor = {name = "Matija"; status = Employed (Fire, Teacher)}


(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane : int}

let update counter = function 
| Fire -> {counter with fire = counter.fire + 1}
| Frost -> {counter with frost = counter.frost + 1}
| Arcane -> {counter with arcane = counter.arcane + 1}

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let count_magic seznam = 
   let rec count_magic_aux acc = function
   | [] -> acc
   | {name; status} :: carovniki -> (
       match status with
       | Newbie -> count_magic_aux acc carovniki
       | Student (vrsta_magije, _) -> count_magic_aux (update acc vrsta_magije) carovniki
       | Employed (vrsta_magije, _) -> count_magic_aux (update acc vrsta_magije) carovniki
   )
in count_magic_aux {fire = 0; frost = 0; arcane = 0} seznam


(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let find_candidate magija specializacija seznam =
       let leto_studija = match specializacija with
       | Historian -> 3
       | Researcher -> 4
       | Teacher -> 5
       in
       let rec search = function
       | [] -> None
       | {name; status} :: xs -> (
              match status with
              | Student (m, y) when m = magija && y >= leto_studija -> Some name
              | _ -> search xs
       )
in search seznam


type 'a gnezdenje =
| Element of 'a
| Podseznam of 'a gnezdenje list

let gnezdenje_primer = Podseznam([Element 1; Element 2; Podseznam([Element 3; Podseznam([Element 4]); Podseznam([])]); Podseznam([Element 5])])

(* let rec najvecja_globina = function
  | Element (_) -> 1
  | Podseznam (t) -> match t with
  | x :: xs -> 1 + max (najvecja_globina [x]) (najvecja_globina xs) *)

  let rec najvecja_globina = function
  | Element (_) -> 1
  | Podseznam (t) -> 1 + List.fold_left (fun a x -> max a (najvecja_globina x)) 0 t

 (* Čuden primer globine z gnezdenjem *) 


 let rec splosci = function
| Element(x) -> [x]
| Podseznam (t) -> List.fold_left (fun a x -> (a @ splosci x)) [] t