let breg1, breg2 =
  ( [|
      [| 60; 50; 40; 30; 20 |];
      [| 40; 50; 60; 73; 80 |];
      [| 10; 20; 30; 40; 50 |];
    |],
    [|
      [| 30; 40; 50; 60; 70 |];
      [| 40; 60; 30; 20; 40 |];
      [| 10; 20; 90; 40; 50 |];
    |] )

let sum list = List.fold_left (fun acc x -> x + acc) 0 list

let rec dodaj_k_krat el list = function
  | 1 -> el :: list
  | k -> el :: dodaj_k_krat el list (k-1)

let extract_bottom_left breg = 
  let n = Array.length breg in
  breg.(n-1).(0)

let remove_bottom_row breg = 
  let n = Array.length breg in
  let new_breg = Array.make (n-1) [| |] in
  for i = 0 to (n-2) do
    new_breg.(i) <- Array.copy breg.(i)
  done;
  new_breg


let remove_first_column breg = 
  let n = Array.length breg in
  let m = Array.length breg.(0) in
  let new_breg = Array.make n [| |] in
  for i = 0 to (n-1) do
    let new_arr = Array.make (m-1) 0 in
    for j = 1 to (m-1) do
      new_arr.(j-1) <- breg.(i).(j)
    done;
    new_breg.(i) <- new_arr
  done;
  new_breg

  type premik = Gor | Desno | Diagonalno

let skoci breg = function  
  | Gor -> breg |> remove_bottom_row 
  | Desno -> breg |> remove_first_column
  | Diagonalno -> breg |> remove_first_column |> remove_bottom_row

let optimalna_pot breg =
  let rec aux pot_acc = function
  | [| [| x |]|] -> (x, List.rev pot_acc)
  | [| arr |] -> let l = arr |> Array.length in
      ((l -1) * (-10) + (arr |> Array.to_list |> sum), List.rev (dodaj_k_krat Desno pot_acc (l-1)))
  | [| [| x |]; [| y |]|] -> (-10 + x + y, List.rev (dodaj_k_krat Gor pot_acc 1) )
  | [| [| x |]; [| y |]; [| z|]|] -> (-20 + x + y + y, List.rev (dodaj_k_krat Gor pot_acc 2))
  | netrivialen_breg -> (extract_bottom_left netrivialen_breg, ) 

in aux [] breg
  (* Mogoče bi bilo bolje, da bi naredil tako, da samo najde vse poti, nato pa najdemo najboljšo*)
  (* Nato pa najdemo maksimalno pot. V resnici bi moral upoštevati, da prvi skok ne more biti diagonalen, vsi ostali so očitno lahko poljubni*)

let najdi_seznam_poti breg = 
  let rec aux acc = function
  | [| [| x |]|] -> List.rev acc
  | [| arr |] -> let l = arr |> Array.length in
      List.rev (dodaj_k_krat Desno acc (l-1))
  | [| [| x |]; [| y |]|] -> List.rev (dodaj_k_krat Gor acc 1) 
  | [| [| x |]; [| y |]; [| z |]|] -> List.rev (dodaj_k_krat Gor acc 2)
  | netrivialen_breg -> aux ()
in aux [] breg


let rec ovrednoti_pot breg = function
| [] -> 0
| Gor :: xs -> extract_bottom_left ((skoci breg1 Gor)) - 12 + ovrednoti_pot (skoci breg1 Gor) xs
| Desno :: xs -> xtract_bottom_left ((skoci breg1 Desno)) - 10 + ovrednoti_pot (skoci breg1 Desno) xs
| Diagonalno :: xs -> xtract_bottom_left ((skoci breg1 Diagonalno)) - 14 + ovrednoti_pot (skoci breg1 Diagonalno) xs

let poti_brez_prve_diag poti = List.filter (fun x :: xs -> x != Diagonalno) poti

(* Tukaj bi morala biti repno rekurzivna funkcija, ki iterira po poteh brez prve diag., najde tisto z največjo energijo *)