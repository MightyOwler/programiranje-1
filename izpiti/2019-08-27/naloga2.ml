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


let rec splosci = function
| Element(x) -> [x]
| Podseznam (t) -> List.fold_left (fun a x -> (a @ splosci x)) [] t

type tribool_prejsnji = Pod | El | Nedoloceno

let alternirajoci_konstruktorji list =
  let rec aux prejsnji = function
  | [] -> true
  | x :: xs -> match x, prejsnji with
    | Element(_), El -> false
    | Podseznam(_), Pod -> false
    | Element(_), (Pod | Nedoloceno) -> true && aux El xs
    | Podseznam(_), (El | Nedoloceno) -> true && aux Pod xs
in
aux Nedoloceno list
