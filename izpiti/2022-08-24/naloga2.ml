type 'a tape = Tape of { left : 'a list; head : 'a; right : 'a list }

type 'a command = Left | Do of ('a -> 'a) | Right

let example = Tape { left = [ 3; 2; 1 ]; head = 4; right = [ 5; 6 ] }

(* 2. a) *)
let map (trak : 'a tape) (f: 'a -> 'b) = match trak with
  | Tape t -> Tape { left = List.map f t.left; head = f t.head; right = List.map f t.right}

(* 2. b) *)

let izvedi trak = 
  match trak with
  | Tape t -> function
    | Left -> if t.left = [] then None else 
      let spremenjen = t.left |> List.rev |> List.hd in
      Some (Tape { left = t.left |> List.rev |> List.tl |> List.rev; head = spremenjen; right = t.head :: t.right})
    | Right -> if t.right = [] then None else 
      Some (Tape { left = t.left @ [t.head]; head = t.right |> List.hd; right = t.right |> List.tl})
    | Do f -> Some (Tape { left = t.left ; head = f t.head; right = t.right})
   

(* 2. c) *)

let rec izvedi_ukaze trak = function
| [] -> trak
| x :: xs -> match trak with
  | Tape t -> match izvedi trak x with 
   | None -> failwith "nemogoče izvesti"
   | Some nov_trak -> izvedi_ukaze nov_trak xs

(* 2. d) *)

let naberi trak ukazi = 
let rec naberi_aux trak acc = function 
| [] -> (acc, trak)
| x :: xs -> match trak with
  | Tape t -> match izvedi trak x with 
   | None -> (List.rev acc, trak)
   | Some nov_trak -> 
      match x with
      | Do f -> naberi_aux nov_trak ((t.head, f t.head) :: acc) xs
      | _ -> naberi_aux nov_trak acc xs
   in 
   naberi_aux trak [] ukazi


(* 2. e) *)

let pripravi_ukaze _ = failwith "TODO"
