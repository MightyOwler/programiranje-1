(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t


(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let grid_to_list grid = 
  let rec loop row_idx col_idx lst = 
    match row_idx, col_idx with
    | row_idx, col_idx when row_idx >= Array.length grid -> lst
    | row_idx, col_idx when col_idx >= Array.length grid.(row_idx) -> loop (row_idx + 1) 0 lst
    | row_idx, col_idx -> loop row_idx (col_idx + 1) (grid.(row_idx).(col_idx) :: lst)
  in
  List.rev (loop 0 0 [])

  (*# chunkify 3 [0; 1; 2; 3; 4; 5; 6; 7; 8];;
  - : int list list = [[0; 1; 2]; [3; 4; 5]; [6; 7; 8]] *)

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

  (* # string_of_list string_of_int " - " [0; 1; 2; 3; 4; 5; 6; 7; 8];;
- : string = "0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8" *)

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

  (* # string_of_nested_list string_of_int "-" ", " [[0; 1; 2]; [3; 4; 5]; [6; 7; 8]];;
- : string = "0-1-2, 3-4-5, 6-7-8" *)

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big
(* 
  print_grid
  (function None -> " " | Some digit -> string_of_int digit)
  state.current_grid  *)

(* Funckija za naključni list, da lahko prevejam spodnje funkcije*)

  let rec random_int_list n = 
    let rec aux n acc = 
      match n with
      | 0 -> acc
      | _ -> aux (n-1) ((Random.int 8) + 1 :: acc)
    in
    aux n [];;

    let rec random_int_option_list n = 
      let rec aux n acc = 
        match n with
        | 0 -> acc
        | _ -> aux (n-1) (Some ((Random.int 8) + 1) :: acc)
      in
      aux n [];;
    
let list_to_grid list =
  list |> chunkify 9 |> List.map Array.of_list |> Array.of_list


      
(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let divison_by_3 x =
  let q = x / 3 in
  let r = x mod 3 in
  (q, r)
let get_box (grid : 'a grid) (box_ind : int) = 
  let (kvocient, ostanek) = divison_by_3(box_ind)
in
Array.init 9 (fun ind -> grid.(3 * ostanek + fst (divison_by_3 ind)).(3 * kvocient + snd (divison_by_3 ind)))

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let coords_to_box grid row_ind col_ind =
  get_box grid (3 * (col_ind / 3) + (row_ind / 3))

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = Array.init 9 (fun row_ind -> Array.map f grid.(row_ind))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit = 
  let string_of_int_option (n : int option) = match n with
| None -> " "
| Some k -> string_of_int k
  in
  print_grid string_of_int_option problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let valid_array array =
  let is_included = Array.make 10 false in
  Array.iter (fun x -> is_included.(x) <- true) array;
  let valid_result = ref true in
  for i = 1 to 9 do
    if is_included.(i) = false then valid_result := false
  done;
  !valid_result;;

let is_valid_solution problem solution = 
  (columns solution) @ (rows solution) @ (boxes solution)
  |> List.map valid_array
  |> List.fold_left ((&&)) true

  