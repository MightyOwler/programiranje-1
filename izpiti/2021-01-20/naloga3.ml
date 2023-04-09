(*============================================================================*]
 Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
 gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
 Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
 fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
 prve fotografije in lokacijo druge fotografije kar se da velik vzpon.

 Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
 vrstnem redu, kot si sledijo po poti. Na primer:

    [350; 230; 370; 920; 620; 80; 520; 780; 630]

 V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
 in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
 Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
 razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
[*============================================================================*)

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
  višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
  utemeljite v komentarju.
[*----------------------------------------------------------------------------*)


let max_of_list list =
  let rec aux max = function
  | [] -> max
  | x :: xs -> if x > max then aux x xs else aux max xs
in
aux (list |> List.hd) list


let najdaljsa_pot list = 
  let rec aux acc subacc = function
  | [] -> subacc :: acc
  | x :: xs -> if x < List.hd subacc then aux (subacc :: acc) [x] xs else aux acc (x :: subacc) xs
  in
  max_of_list (List.map (fun lst -> (lst |> List.hd) - (lst |> List.rev |> List.hd)) (aux [] [List.hd list] list))

  (* Enkrat se gremo čez seznam, da ustvarimo nove sezname. To je vse konstantno. Nato izračunamo razliko med največjo in najmanjšo komponento v seznamih. To je linearno. Poleg tega je linearen maksimum na seznamih
     Torej imamo v resnici O(n).
  *)


(* b *)(*----------------------------------------------------------------------------*]
  Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
  tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
[*----------------------------------------------------------------------------*)

