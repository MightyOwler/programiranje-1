type available = { loc : int * int; possible : int list}
type enumerated_avialable_list = { digit : int; coords_list : (int * int) list}
type state = { problem : Model.problem; current_grid : int option Model.grid; available_list: available list}
let sudoku_numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]

let states_table = Hashtbl.create 100;;
let cached_state (state : state) = 
  Hashtbl.add states_table state (); 
  if (Hashtbl.mem states_table state) then false else true

let test_grid = Model.list_to_grid (Model.random_int_list 81);;
let test_option_grid = [|[|None; Some 6; Some 6; Some 2; Some 4; Some 1; Some 2; Some 5; Some 4|];
[|Some 1; Some 2; Some 7; Some 5; Some 7; Some 3; Some 4; Some 7; Some 2|];
[|Some 8; Some 2; Some 7; Some 4; Some 7; Some 7; Some 8; Some 4; Some 2|];
[|Some 2; Some 8; Some 3; Some 3; Some 8; Some 1; Some 6; Some 1; Some 3|];
[|Some 5; Some 6; Some 4; Some 7; Some 4; Some 2; Some 7; Some 8; Some 8|];
[|Some 7; Some 1; Some 5; Some 3; Some 5; Some 7; Some 2; Some 6; Some 3|];
[|Some 4; Some 2; Some 4; Some 3; Some 2; Some 2; Some 7; Some 4; Some 4|];
[|Some 5; Some 8; Some 6; Some 2; Some 6; Some 5; Some 7; Some 3; Some 2|];
[|Some 7; Some 2; Some 4; Some 7; Some 1; Some 3; Some 5; Some 6; None|]|];;

let nonzeroAvailables availableList = List.filter (fun x -> List.length x.possible > 0) availableList

let remove_singular_availables available_list = List.filter (fun x -> List.length x.possible > 1) available_list

(* singularAvailables je mišljeno availabli, ki imajo singleton v possible parametru. *)
let singularAvailables availableList = List.filter (fun x -> List.length x.possible = 1) availableList

(* Pogledamo, ali je vstavljanje števila na določeno koordinato mreže legalno*)

let check_number_legality grid row_ind col_ind n =
  [Model.get_row grid row_ind; Model.get_column grid col_ind;
  Model.coords_to_box grid row_ind col_ind] 
  |> List.map Array.to_list |> List.concat |> List.mem (Some n) |> not

let coords_to_available row_ind col_ind grid (n : int option) =
  match n with
  | Some _ -> {loc = (row_ind, col_ind); possible = []}
  | None -> let possible_list = List.filter (fun n -> check_number_legality grid row_ind col_ind n) sudoku_numbers in 
            {loc = (row_ind, col_ind); possible = possible_list}

(* To bi bilo treba narediti prek available lista, ne pa grida! *)
  let grid_to_avail_list grid =
  let foldaj row_ind col_ind (n : int option) (acc : available list) = 
    (coords_to_available row_ind col_ind grid n) :: acc
  in
    (Model.foldi_grid foldaj grid []) |> List.rev |> nonzeroAvailables

let test_a_list = [{loc = (1, 2); possible = [1; 5; 3]}; {loc = (2, 3); possible = [2; 4; 3]}; {loc = (3, 6); possible = [3; 8; 1]}]

let findSmallestAvailable avail_list = 
  let rec aux smallest = function
      | [] -> smallest
      | x :: xs -> 
          match x.possible |> List.length with
          | 2 -> x
          | k when k < (smallest.possible |> List.length) -> aux x xs
          | _ -> aux smallest xs
            in
  aux (avail_list |> List.hd) avail_list

let avail_to_enumetated_avail avail_list = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> 
      let number_list = List.filter (fun avail -> List.mem x avail.possible) avail_list in 
      if number_list = [] then aux acc xs else
      aux ({digit = x; coords_list = List.map (fun avail -> avail.loc) number_list} :: acc) xs
  in
  aux [] sudoku_numbers

let findSmallestEnumeratedAvailable enum_avail_list = 
  let rec aux smallest = function
      | [] -> smallest
      | x :: xs -> 
          match x.coords_list |> List.length with
          | 1 | 2 -> x
          | k when k < (smallest.coords_list |> List.length) -> aux x xs
          | _ -> aux smallest xs
            in
  aux (enum_avail_list |> List.hd) enum_avail_list

let insert_into_a_grid grid row_ind col_ind value =
  let new_grid = Model.copy_grid grid in
    (new_grid).(row_ind).(col_ind) <- value;
  new_grid

let solve_trivial_cells grid available_list=
  let singular_availables = available_list |> singularAvailables in
  if singular_availables = [] then grid 
  else
  List.hd (List.map (fun avail ->
    let (prvi, drugi) = avail.loc in
   insert_into_a_grid grid prvi drugi  (Some (List.hd avail.possible))) singular_availables)

  (* Vzamemo nek grid, ki ga dobimo iz state.current_grid, in mu zapolnimo trivialne celice.*)

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "-" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_list = problem.initial_grid |> grid_to_avail_list}

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
      let (x, y) = avail.loc in
      let new_grid = insert_into_a_grid state.current_grid x y guess in
      Some ({state with current_grid = new_grid; available_list = []}, {state with available_list = []})
| _ ->
    (* Če imamo vsaj 2 availabla*)
    (* Če imamo kake trivialne številke, potem preučuje samo to možnost*)
    if (state.available_list |> List.length) != (state.available_list |> remove_singular_availables |> List.length) then
      (* V tem primeru samo vstavimo trivialne, ni treba razvejevati*)
      let trivialy_corrected = solve_trivial_cells state.current_grid state.available_list in
      Some ({state with current_grid = trivialy_corrected; available_list = trivialy_corrected |> grid_to_avail_list }, {state with available_list = []})
    else
        let first_avail = state.available_list |> findSmallestAvailable in
        let (x, y) = first_avail.loc in
        let first_guess = Some (first_avail.possible |> List.hd) in
        let new_grid_1 = insert_into_a_grid state.current_grid x y first_guess in
        let new_available_list_1 = new_grid_1 |> grid_to_avail_list in

        match first_avail.possible |> List.length with
          | 2 ->
            let second_guess = Some (first_avail.possible |>  List.rev |> List.hd) in
            let new_grid_2 = insert_into_a_grid state.current_grid x y second_guess in
            let new_available_list2 = new_grid_2 |> grid_to_avail_list in
    
            Some ({state with current_grid = new_grid_1; available_list = new_available_list_1}, {state with current_grid = new_grid_2; available_list = new_available_list2})
          | _ -> (* V tem primeru moramo en element parametra possible odstraniti. *)

          (* Tole enostavno ni najbolj optimalno, ne prihrani veliko časa*)
          let opt = state.available_list |> avail_to_enumetated_avail |> findSmallestEnumeratedAvailable in
          let opt_digit = Some opt.digit in
          let coords_lst = opt.coords_list in
          match List.length coords_lst with
          | 1 ->
            let (x, y) = List.hd coords_lst in
            let new_grid = insert_into_a_grid state.current_grid x y opt_digit in
            Some ({state with current_grid = new_grid; available_list = new_grid |> grid_to_avail_list }, {state with available_list = []})
          | 2 ->
            let (x_1, y_1) = List.hd coords_lst in
            let (x_2, y_2) = coords_lst |> List.rev |> List.hd in
            let new_grid_1 = insert_into_a_grid state.current_grid x_1 y_1 opt_digit in
            let new_grid_2 = insert_into_a_grid state.current_grid x_2 y_2 opt_digit in
            Some ({state with current_grid = new_grid_1; available_list = new_grid_1 |> grid_to_avail_list },
            {state with current_grid = new_grid_2; available_list = new_grid_2 |> grid_to_avail_list })
          | _ -> 

          let new_available_list_2 = match state.available_list with
          | x :: xs -> {x with possible = first_avail.possible |> List.tl} :: xs
          | _ -> assert false
            in
          Some ({state with current_grid = new_grid_1; available_list = new_available_list_1}, {state with available_list = new_available_list_2})
        

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
