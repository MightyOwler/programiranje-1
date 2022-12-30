type available = { loc : int * int; possible : int list}

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; available_grid: available Model.grid }

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

(* To zdaj dela. 
# check_number_legality Model.test_option_grid 5 5 5;;
- : bool = false *)

let get_available_grid (grid : int option Model.grid) =
  let foldaj row_ind col_ind (n : int option) (acc : available list) = 
    acc @ [(coords_to_available row_ind col_ind grid n)]
  in
    Model.list_to_grid (Model.foldi_grid foldaj grid [])

let match_trivial (n : int option) (avail : available) =
  match n, avail.possible with
  | Some x, _ -> Some x
  | _, []  -> None
  | _, [x] -> Some x
  | _ -> None

let solve_trivial_cells (grid : int option Model.grid) =
  let listified_current = Model.grid_to_list grid in
  let listified_available = Model.grid_to_list (get_available_grid grid) in
  Model.list_to_grid (List.map2 match_trivial listified_current listified_available)

  (* Vzamemo nek grid, ki ga dobimo iz state.current_grid, in mu zapolnimo trivialne celice.*)

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "-" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_grid = get_available_grid problem.initial_grid}

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
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)

  (* Tole se gotovo da izboljšati. Zaenkrat niti še ne dela prav, treba bo še dosti popraviti. Namreč če je enaka, bo treba insertati kar eno vrednost. *)
  let trivialy_corrected = solve_trivial_cells state.current_grid in
  if state.current_grid != trivialy_corrected then
    Some ({state with current_grid = trivialy_corrected; available_grid = get_available_grid trivialy_corrected}, {state with available_grid = get_available_grid trivialy_corrected})
  else
    (* Tukaj lahko dam None v primeru, ko je pogoj izpoljnen, vendar ni pravilna rešitev*)
    Some ({state with current_grid = trivialy_corrected; available_grid = (Model.copy_grid state.available_grid)}, {state with available_grid = (Model.copy_grid state.available_grid)})


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
