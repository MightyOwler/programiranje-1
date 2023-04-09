type 'a operator = 'a -> 'a -> 'a

type 'a atom = Value of 'a | Operator of 'a operator

type 'a term = 'a atom list

type 'a result = Finished of 'a | Term of 'a term | Error

let plus = Operator ( + )

let krat = Operator ( * )
let float_krat = Operator ( *. )

let deljeno = Operator ( / )
let float_deljeno = Operator ( /. )

let minus = Operator ( - )

let primer : int term = [ Value 3; Value 4; plus; Value 5; deljeno ]    

(* 2. a) *)

let primer1 : int term = [ Value 1; Value 2; plus; Value 4; minus; Value 5; krat]

let primer2 : float term = [ Value 5.3; Value 4.6; float_deljeno; Value 1.7; float_krat]

let napacni_primer = [Value 0; plus; deljeno]

(* 2. b) *)

let korak izraz =
  match izraz with
  | [Value x] -> Finished x
  | Value x :: Value y :: Operator o :: xs ->  Term  (Value (o x y) :: xs)
  | _ -> failwith "do tega ne pride"
  

(* 2. c) *)

let rec izvedi = function
 | [Value x] -> Some (Finished x)
 | Value x :: Value y :: Operator o :: xs -> izvedi (Value (o x y) :: xs)
 | _ -> None


(* 2. d) *)

(* To je precej naiven način gledanja, je pa res, da nič ne računa, tako kot zahtevajo navodila*)
let valid izraz =
  let rec aux = function
  | [] | [_] -> true
  | Value x :: Value y :: Operator o :: xs -> aux (Value x :: xs)
  | _ -> false 
in 
aux izraz

(* 2. e) *)

let combine vrednosti operatorji = 
  if List.length vrednosti = 1 + List.length operatorji then
    let rec aux acc v o  = match v, o with
    | [], [] -> Some acc
    | x :: xs, Operator y :: ys -> aux (acc @ [Value x; Operator y]) xs ys
    | _ -> None
  in
  aux [Value (vrednosti |> List.hd)] (vrednosti |> List.tl) operatorji
  else
    None
