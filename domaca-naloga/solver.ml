type available = { loc : int * int; possible : int list}

(* TODO: V resnici tako kot sem naredil ni najbolj smiselno, dosti bolje bi bilo imeti available_list, saj tip avaiblable že poskrbi, da imamo znano koordinato.
   Potem lahko namesto da se mučim z available gridom samo manjšam list in tudi preverjam glede na ta list.*)
type state = { problem : Model.problem; current_grid : int option Model.grid; available_list: available list}

let states_table = Hashtbl.create 100;;
let cached_state (state : state) = 
  Hashtbl.add states_table state (); 
  if (Hashtbl.mem states_table state) then false else true

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

let find_best_avails avail_list = 
  (* Če imamo kak available z dolžino possible 2, lahko takoj razrešimo na 2 primera. Sicer moramo biti pazljivi!! Namreč moramo razvejiti tako, da zmanjšamo avail list!*)
      match orderAvailablesByPossible avail_list with
      | x :: y :: xs -> if (x.possible |> List.length) = 2 then (x,x) else (x, y)
      | _ -> failwith "Do tega ne more priti, ker imamo vedno vsaj 2"

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


let branch_state (state : state) : (state * state) option = 
  if cached_state state then None else
  match state.available_list |> List.length with
| 0 -> None
| 1 -> 
  (* Razdelimo glede na dolžino available_lista. *)
      let first_avail = state.available_list |> List.hd in
      let first_guess = Some (List.hd first_avail.possible) in
      let (x, y) = first_avail.loc in
      let new_grid = insert_into_a_grid state.current_grid x y first_guess in
  (* V tem primeru je itak že rešen sudoku*)
      Some ({state with current_grid = new_grid; available_list = []}, {state with available_list = []})
| _ ->
     (* Če imamo vsaj 2 availabla*)
    let trivialy_corrected = state.current_grid |> solve_trivial_cells in
    (* Če imamo kake trivialne številke, potem preučuje samo to možnost*)
    if state.current_grid != trivialy_corrected then
      (* V tem primeru samo vstavimo trivialne, ni treba razvejevati*)
      Some ({state with current_grid = trivialy_corrected; available_list = trivialy_corrected |> get_available_list_from_grid }, {state with available_list = []})
    else
      let (first_avail, second_avail) = state.available_list |> find_best_avails in
      let (x, y) = first_avail.loc in
      let first_guess = Some (first_avail.possible |> List.hd) in
      let new_grid1 = insert_into_a_grid state.current_grid x y first_guess |> solve_trivial_cells in
      let new_available_list1 = new_grid1 |> get_available_list_from_grid in

      match first_avail = second_avail with
      | true ->
        let second_guess = Some (second_avail.possible |>  List.rev |> List.hd) in
        let new_grid2 = insert_into_a_grid state.current_grid x y second_guess |> solve_trivial_cells in
        let new_available_list2 = new_grid2 |> get_available_list_from_grid in

         Some ({state with current_grid = new_grid1; available_list = new_available_list1}, {state with current_grid = new_grid2; available_list = new_available_list2})
      | false -> (* V tem primeru moramo en possible odstraniti. *)
        let new_possible = first_avail.possible |> List.tl in
        let new_available_list2 = match state.available_list with
        | x :: xs -> {x with possible = new_possible} :: xs
        | _ -> [] (* Do tega itak ne pride*)
          in
        Some ({state with current_grid = new_grid1; available_list = new_available_list1}, {state with available_list = new_available_list2})
        

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
