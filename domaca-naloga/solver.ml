type available = { loc : int * int; possible : int list}

(* TODO: V resnici tako kot sem naredil ni najbolj smiselno, dosti bolje bi bilo imeti available_list, saj tip avaiblable že poskrbi, da imamo znano koordinato.
   Potem lahko namesto da se mučim z available gridom samo manjšam list in tudi preverjam glede na ta list.*)
type state = { problem : Model.problem; current_grid : int option Model.grid; available_list: available list}

(* Pogledamo, ali je vstavljanje števila na določeno koordinato mreže legalno*)
let check_number_legality (grid : int option Model.grid) row_ind col_ind n =
  [Model.get_row grid row_ind; Model.get_column grid col_ind;
  Model.coords_to_box grid row_ind col_ind] 
  |> List.map Array.to_list |> List.concat |> List.mem (Some n) |> not

let sudoku_numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let coords_to_available row_ind col_ind (grid : int option Model.grid) (n : int option) =
  let possible_list = List.filter (fun n -> check_number_legality grid row_ind col_ind n) sudoku_numbers
  in 
  match n with
  | None -> {loc = (row_ind, col_ind); possible = possible_list}
  | Some _ -> {loc = (row_ind, col_ind); possible = []}

(* Morda se da optimizirati generiranje novih gridov *)

let nonzeroAvailables availableList = List.filter (fun x -> List.length x.possible > 0) availableList

let rec orderAvailablesByPossible lst = 
  match lst with
  | [] -> []
  | hd::tl -> 
      let left = orderAvailablesByPossible (List.filter (fun x -> List.length x.possible < List.length hd.possible) tl) in
      let right = orderAvailablesByPossible (List.filter (fun x -> List.length x.possible >= List.length hd.possible) tl) in
      left @ [hd] @ right;;

let get_available_list_from_grid (grid : int option Model.grid) =
  let foldaj row_ind col_ind (n : int option) (acc : available list) = 
    acc @ [(coords_to_available row_ind col_ind grid n)]
  in
    (Model.foldi_grid foldaj grid []) |> nonzeroAvailables |> orderAvailablesByPossible

  (* singularAvailables je mišljeno availabli, ki imajo singleton v possible parametru. *)
let singularAvailables availableList = List.filter (fun x -> List.length x.possible = 1) availableList

let insert_into_a_grid grid row_ind col_ind value =
  let new_grid = Model.copy_grid grid in
    (new_grid).(row_ind).(col_ind) <- value;
  new_grid

let solve_trivial_cells grid =
  let singular_availables = grid |> get_available_list_from_grid |> singularAvailables in
  if singular_availables = [] then grid 
  else
  List.hd (List.map (fun avail ->
    let (prvi, drugi) = avail.loc in
   insert_into_a_grid grid prvi drugi  (Some (List.hd avail.possible))) singular_availables)

  (* Vzamemo nek grid, ki ga dobimo iz state.current_grid, in mu zapolnimo trivialne celice.*)


  (* To vzame nek available in najde njegove najbolj ustrezne elemente*)
let find_best_possible grid avail_element = 
  let (x_loc, y_loc) = avail_element.loc in
  let ordered_list = 
  List.sort (fun m n -> 
    let m_score = insert_into_a_grid grid x_loc y_loc (Some m) |> get_available_list_from_grid |> List.length in
    let n_score = insert_into_a_grid grid x_loc y_loc (Some n) |> get_available_list_from_grid |> List.length in 
  m_score - n_score) avail_element.possible in
  match ordered_list with
  | (x :: y :: xs) -> (Some x, Some y)
  | _ -> failwith "Do tega sploh ne bi smelo priti, saj imamo v tem primeru trivialne available"

(* Ti funkciji v resnici predstavljata eno in isto, kasneje bom morda opustil prvo. Tudi ni ravno v duhu funckijskega programiranja imeti preveč *)
let find_best_possible_list grid avail_list = 
  let available_to_tuples avail_element = 
    List.map (fun x -> (avail_element.loc, x)) avail_element.possible in 
  let list_of_tuples = avail_list |> List.map available_to_tuples |> List.concat in
  let ordered_list = 
    List.sort (fun ((x,y), m) ((x',y'), n) -> 
      let m_score = insert_into_a_grid grid x y (Some m) |> get_available_list_from_grid |> List.length in
      let n_score = insert_into_a_grid grid x' y' (Some n) |> get_available_list_from_grid |> List.length in 
    m_score - n_score) list_of_tuples in
    match ordered_list with
  | ((x, m) :: (y, n) :: xs) -> ({loc = x; possible = [m]}, {loc = x; possible = [n]})
  | _ -> failwith "Do tega sploh ne bi smelo priti, saj imamo v tem primeru trivialne available"
  


let find_best_guesses grid avail_list = match avail_list with
  | [] -> failwith "To se ne more zgoditi, available list nikoli nima praznih"
  | x :: [] -> find_best_possible_list grid [x]
  (* V tem primeru je samo treba najti najboljša elementa, po predpostavkah res imamo dva taka elementa *)
  | x :: y :: xs -> let optimal_length = x.possible |> List.length in
    let opt_availables = List.filter (fun x -> List.length x.possible = optimal_length) avail_list in
    match opt_availables |> List.length with
    | 1 -> find_best_possible_list grid [x]
    | _ -> find_best_possible_list grid avail_list (* Zanekrat samo za poskušino, da vidim ali ta ideja sploh dela. Seveda je v tem primeru treba najti boljšo funkcijo. *)


let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "-" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_list = problem.initial_grid |> get_available_list_from_grid }

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

let branch_state (state : state) : (state * state) option = match state.available_list |> List.length with
| 0 -> None
| 1 -> 
  (* Razdelimo glede na dolžino available_lista. *)
      let first_avail = state.available_list |> List.hd in
      let first_guess = Some (List.hd first_avail.possible) in
      let (x, y) = first_avail.loc in
      let new_random_grid = insert_into_a_grid state.current_grid x y first_guess in
  (* V tem primeru je itak že rešen sudoku*)
      Some ({state with current_grid = new_random_grid; available_list = []}, {state with available_list = []})
| _ ->
    let trivialy_corrected = state.current_grid |> solve_trivial_cells in
    (* Če imamo kake trivialne številke, potem preučuje samo to možnost*)
    if state.current_grid != trivialy_corrected then
      (* V tem primeru samo vstavimo trivialne, ni treba razvejevati*)
      Some ({state with current_grid = trivialy_corrected; available_list = trivialy_corrected |> get_available_list_from_grid }, {state with available_list = []})
    else
      (* Zakaj vzamemo first avail? To je treba dati v pomožno funckijo!! In sicer: pogledamo tiste z najmanjšim avail.possible in izmed tistih najdemo najbolj ugodnega!*)
      let (first_avail, second_avail) = state.available_list |> find_best_possible_list state.current_grid in
      let (x, y) = first_avail.loc in
      let (x', y') = second_avail.loc in
      let first_guess = Some (first_avail.possible |> List.hd) in
      let second_guess = Some (second_avail.possible |> List.hd) in
      (* Tukaj je treba popraviti first_guess in second_guess. Namreč seveda ni najbolje vzeti kar prvi in drugi element, ampak je bolje najprej pogledati, pri katerem elementu bo najbolje razpadlo*)
      (* Se da tukaj morda vpeljati kak while? Da se vedno takoj znebimo trivialnih?*)
      let new_grid1 = insert_into_a_grid state.current_grid x y first_guess |> solve_trivial_cells in
      let new_grid2 = insert_into_a_grid state.current_grid x' y' second_guess |> solve_trivial_cells in
      let new_available_list1 = new_grid1 |> get_available_list_from_grid in
      let new_available_list2 = new_grid2 |> get_available_list_from_grid in
      (* Verjetno se da optimizirazi z memoizacijo.*)
      Some ({state with current_grid = new_grid1; available_list = new_available_list1}, {state with current_grid = new_grid2; available_list = new_available_list2})

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
