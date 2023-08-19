type available = { loc : int * int; possible : int list}
type state = {problem : Model.problem; current_grid : int option Model.grid; available_list: available list}
let sudoku_numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]

(* Preprosta memoizacija *)
let states_table = Hashtbl.create 1000;;
let cached_state (state : state) = 
  Hashtbl.add states_table state (); 
  if (Hashtbl.mem states_table state) then false else true

(* Filtrira available list tako, da odstrani tiste, ki majo dolžino possible argumenta 0. *)
let nonzero_availables availableList = List.filter (fun x -> List.length x.possible > 0) availableList

(* Filtrira available list tako, da ohrani samo tiste, ki imajo dolžino possible argumenta 1. *)
let singleton_availables availableList = List.filter (fun x -> List.length x.possible = 1) availableList

(* Vrne bool vrednost, ki določa legalnost vstavljanja števila na določeno koordinato mreže *)
let check_number_legality grid row_ind col_ind n =
  let is_n x = x = Some n in
  let row = Model.get_row grid row_ind in
  let col = Model.get_column grid col_ind in
  let box = Model.coords_to_box grid row_ind col_ind in

  not (Array.exists is_n row || Array.exists is_n col || Array.exists is_n box)

(* Pretvori koordinate in mrežo v available element *)
let coords_to_available row_ind col_ind grid (n : int option) =
  match n with
  | Some _ -> {loc = (row_ind, col_ind); possible = []}
  | None -> let possible_list = List.filter (fun n -> check_number_legality grid row_ind col_ind n) sudoku_numbers in 
            {loc = (row_ind, col_ind); possible = possible_list}


(* Pretvori mrežo v available list. Ta funkcija se izvede samo čisto na začetku reševanja sudokuja *)
let grid_to_avail_list grid =
  let foldaj row_ind col_ind (n : int option) acc = 
    let new_avail = coords_to_available row_ind col_ind grid n in
    if new_avail.possible = [] then 
    acc
  else 
    new_avail :: acc
  in
    (Model.foldi_grid foldaj grid [])

(* Preveri, če obstaja v abailable listu kak element, ki ima singleton za possible arugment *)
let has_singleton available_list =
  List.exists (fun avl -> match avl.possible with [_] -> true | _ -> false) available_list


(* Iz available lista najde available element, ki se zdi najbolj ustrezen *)
let find_smallest_available avail_list = 
  let rec aux smallest = function
      | [] -> smallest
      | x :: xs -> 
          match x.possible |> List.length with
          | 2 -> x
          | k when k < (smallest.possible |> List.length) -> aux x xs
          | _ -> aux smallest xs
            in
  aux (avail_list |> List.hd) avail_list

(* Vstavi element v mrežo *)
let insert_into_grid grid (row_ind, col_ind) value =
  let new_grid = Model.copy_grid grid in
    (new_grid).(row_ind).(col_ind) <- value;
  new_grid

(* Pomožna funkcija, updata available element *)
let remove_from_possible loc value avail =
  let (x, y) = loc in
  let (ax, ay) = avail.loc in
  if x = ax (* Same row *)
  || y = ay (* Same column *)
  || (x / 3 = ax / 3 && y / 3 = ay / 3) (* Same 3x3 square *)
  then {avail with possible = List.filter ((<>) value) avail.possible}
  else avail

(* Updata celoten available list s prejšnjo funkcijo *)
let update_available_list available_list loc value =
  List.map (remove_from_possible loc value) available_list

(* Odstrani available z izbrano lokacijo *)
let remove_available_by_loc location available_list =
  List.filter (fun avail -> avail.loc <> location) available_list

(* Posodobi available list glede na izbrano sudoku potezo *)
let update_and_remove_singletons available_list =
  let singletons = singleton_availables available_list in
  let rec update sings acc_list = 
    match sings with
    | [] -> acc_list
    | x :: xs -> 
        (match x.possible with 
          | [v] -> 
              let updated_list = update_available_list acc_list x.loc v in
              update xs updated_list
          | _ -> assert false)
  in update singletons available_list
  
(* Izploni trivialne celice v mreži *)
let solve_trivial_cells grid available_list =
  let singular_availables = singleton_availables available_list in
  if singular_availables = [] then grid
  else
      List.fold_left (fun current_grid avail -> 
          insert_into_grid current_grid avail.loc (Some (List.hd avail.possible))
      ) grid singular_availables
let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "-" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_list = problem.initial_grid |> grid_to_avail_list |> nonzero_availables}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state


let branch_state (state : state) : (state * state) option = 
  if cached_state state then None else
  (* Razdelimo glede na dolžino available_lista. *)
  match state.available_list |> List.length with
| 0 -> None
| 1 -> 
  (* V tem primeru je itak že rešen sudoku*)
  let avail = state.available_list |> List.hd in
  let guess = Some (List.hd avail.possible) in
  let new_grid = insert_into_grid state.current_grid avail.loc guess  in
  Some ({state with current_grid = new_grid; available_list = []}, {state with available_list = []})
| _ ->
  (* Če imamo vsaj 2 availabla v listu*)
  
  if state.available_list |> has_singleton then
    (* Če imamo kake singletone, jih vstavimo*)
    let trivially_corrected = solve_trivial_cells state.current_grid state.available_list in
    Some ({state with current_grid = trivially_corrected; available_list = state.available_list |> update_and_remove_singletons |> nonzero_availables }, {state with available_list = []})
  else
    (* Sicer pogledamo dolžino najkrajšega avaialable argumenta *)
      let smallest_avail = state.available_list |> find_smallest_available in
      let first_guess = Some (smallest_avail.possible |> List.hd) in
      let new_grid_1 = insert_into_grid state.current_grid smallest_avail.loc first_guess in
      let new_available_list_1 = remove_available_by_loc smallest_avail.loc (update_available_list state.available_list smallest_avail.loc (smallest_avail.possible |> List.hd))  in
      match smallest_avail.possible |> List.length with
        | 2 ->
          let second_guess = Some (smallest_avail.possible |> List.rev |> List.hd) in
          let new_grid_2 = insert_into_grid state.current_grid smallest_avail.loc second_guess in
          let new_available_list_2 = remove_available_by_loc smallest_avail.loc (update_available_list state.available_list smallest_avail.loc (smallest_avail.possible |>  List.rev |> List.hd)) in
          Some ({state with current_grid = new_grid_1; available_list = new_available_list_1 |> nonzero_availables}, {state with current_grid = new_grid_2; available_list = new_available_list_2 |> nonzero_availables})
        | _ -> (* V tem primeru moramo en element parametra possible odstraniti. *)
        let preostanek_possibla = smallest_avail.possible |> List.tl in
        let new_available_list_2 =
          if List.length state.available_list = 0 then
            assert false
          else
            let update_avail i avail =
              if i = 0 then
                {avail with possible = preostanek_possibla}
              else
                avail
            in
            List.mapi update_avail state.available_list
          in
        Some ({state with current_grid = new_grid_1; available_list = new_available_list_1 |> nonzero_availables}, {state with available_list = new_available_list_2 |> nonzero_availables})
        

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)

  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state