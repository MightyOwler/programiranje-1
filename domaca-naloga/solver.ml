type available = { loc : int * int; possible : int list}
type enumerated_avialable_list = { digit : int; coords_list : (int * int) list}
type state = { problem : Model.problem; current_grid : int option Model.grid; available_list: available list}
let sudoku_numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]

let example_available_list = [
    { loc = (0, 0); possible = [1; 2; 3] };
    { loc = (0, 1); possible = [1; 4] };
    { loc = (0, 2); possible = [5] };
    { loc = (1, 0); possible = [6; 7; 8] };
    { loc = (1, 1); possible = [2; 5; 9] };
    { loc = (1, 2); possible = [1; 3] };
    { loc = (2, 0); possible = [4; 7] };
    { loc = (2, 1); possible = [6; 8] };
    { loc = (2, 2); possible = [2; 9] };
    { loc = (3, 0); possible = [3; 5; 7] }
]

let states_table = Hashtbl.create 1000;;
let cached_state (state : state) = 
  Hashtbl.add states_table state (); 
  if (Hashtbl.mem states_table state) then false else true

let test_grid = Model.list_to_grid (Model.random_int_list 81);;
let test_option_grid = [|[|None; Some 6; Some 6; Some 2; Some 4; Some 1; Some 2; Some 5; Some 4|];
[|Some 1; Some 2; Some 7; Some 5; Some 7; Some 3; Some 4; Some 7; Some 2|];
[|Some 8; Some 2; Some 7; Some 4; None; Some 7; Some 8; None; Some 2|];
[|Some 2; Some 8; Some 3; Some 3; Some 8; Some 1; Some 6; Some 1; Some 3|];
[|Some 5; Some 6; Some 4; None; Some 4; Some 2; Some 7; Some 8; Some 8|];
[|Some 7; Some 1; Some 5; Some 3; Some 5; None; Some 2; Some 6; Some 3|];
[|Some 4; Some 2; None; Some 3; Some 2; Some 2; Some 7; Some 4; Some 4|];
[|Some 5; Some 8; Some 6; Some 2; None; Some 5; Some 7; Some 3; Some 2|];
[|Some 7; Some 2; Some 4; Some 7; Some 1; Some 3; Some 5; Some 6; None|]|];;


(* Printaj available array *)
let print_available_list arr =
  Array.iter (fun av ->
    let (x, y) = av.loc in
    Printf.printf "Location: (%d, %d) | Possible: [" x y;
    List.iter (fun p -> Printf.printf "%d; " p) av.possible;
    print_endline "]"
  ) arr

let nonzeroAvailables availableList = List.filter (fun x -> List.length x.possible > 0) availableList

let remove_singular_availables available_list =
  let add_non_singular acc avail =
    if List.length avail.possible > 1 then avail :: acc else acc
  in
  Array.fold_left add_non_singular [] available_list |> Array.of_list

(* singularAvailables je mišljeno availabli, ki imajo singleton v possible parametru. *)
let singularAvailables availableList = List.filter (fun x -> List.length x.possible = 1) availableList

(* Pogledamo, ali je vstavljanje števila na določeno koordinato mreže legalno*)

(* let check_number_legality grid row_ind col_ind n =
  [Model.get_row grid row_ind; Model.get_column grid col_ind;
  Model.coords_to_box grid row_ind col_ind] 
  |> List.map Array.to_list |> List.concat |> List.mem (Some n) |> not *)

let check_number_legality grid row_ind col_ind n =
  let is_n x = x = Some n in
  let row = Model.get_row grid row_ind in
  let col = Model.get_column grid col_ind in
  let box = Model.coords_to_box grid row_ind col_ind in

  not (Array.exists is_n row || Array.exists is_n col || Array.exists is_n box)

let coords_to_available row_ind col_ind grid (n : int option) =
  match n with
  | Some _ -> {loc = (row_ind, col_ind); possible = []}
  | None -> let possible_list = List.filter (fun n -> check_number_legality grid row_ind col_ind n) sudoku_numbers in 
            {loc = (row_ind, col_ind); possible = possible_list}



    let grid_to_avail_list grid =
      let foldaj row_ind col_ind (n : int option) acc = 
        let new_avail = coords_to_available row_ind col_ind grid n in
        if new_avail.possible = [] then 
        acc
      else 
        new_avail :: acc
      in
        (Model.foldi_grid foldaj grid [])
    
let has_singleton available_list =
  List.exists (fun avl -> match avl.possible with [_] -> true | _ -> false) available_list

let filter_singletons available_list =
  List.filter (fun avl -> match avl.possible with | [_] -> true | _ -> false) available_list

let test_a_list = test_option_grid |> grid_to_avail_list

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

(* TODO ugotovi, kako lahko tu dejansko uporabiš lastnost, da lahko grid spreminjaš brez kopiranja*)

let insert_into_grid grid (row_ind, col_ind) value =
  let new_grid = Model.copy_grid grid in
    (new_grid).(row_ind).(col_ind) <- value;
  new_grid


  let remove_from_possible loc value avail =
    let (x, y) = loc in
    let (ax, ay) = avail.loc in
    if x = ax (* Same row *)
    || y = ay (* Same column *)
    || (x / 3 = ax / 3 && y / 3 = ay / 3) (* Same 3x3 square *)
    then {avail with possible = List.filter ((<>) value) avail.possible}
    else avail
  
  let update_available_list available_list loc value =
    List.map (remove_from_possible loc value) available_list

    let apply_single_possible available_list =
      let apply_single avl acc_list =
        match avl.possible with
        | [single_value] -> update_available_list acc_list avl.loc single_value
        | _ -> acc_list
      in
      List.fold_right apply_single available_list available_list
  
      let update_list_with_singletons available_list =
        let singletons = filter_singletons available_list in
        List.fold_left 
          (fun acc_list singleton ->
             let value = List.hd singleton.possible in
             update_available_list acc_list singleton.loc value) 
          available_list singletons
  
(* Morda lahko nekoliko razširim trivialno reševanje... *)
let solve_trivial_cells grid available_list=
  let singular_availables = available_list |> singularAvailables in
  if singular_availables = [] then grid 
  else
  List.hd (List.map (fun avail ->  insert_into_grid grid avail.loc  (Some (List.hd avail.possible))) singular_availables)
   

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
| 0 -> (*print_endline "primer 0";*) None
| 1 -> 
      (* V tem primeru je itak že rešen sudoku*)
      (* print_endline "primer 1"; *)
      let avail = state.available_list |> List.hd in
      let guess = Some (List.hd avail.possible) in
      let new_grid = insert_into_grid state.current_grid avail.loc guess  in
      Some ({state with current_grid = new_grid; available_list = []}, {state with available_list = []})
| _ ->
    (* print_endline "primer 2"; *)
    (* Če imamo vsaj 2 availabla*)
    (* Če imamo kake trivialne številke, potem preučuje samo to možnost*)
    (* Primerjaš lahko samo available! *)
    
    if state.available_list |> has_singleton then
      (* V tem primeru samo vstavimo trivialne, ni treba razvejevati*)
      let trivially_corrected = solve_trivial_cells state.current_grid state.available_list in
      print_endline "trivialno popravljam";
      Some ({state with current_grid = trivially_corrected; available_list = trivially_corrected |> grid_to_avail_list|> nonzeroAvailables }, {state with available_list = []})
    else
        let first_avail = state.available_list  |> findSmallestAvailable in
        let first_guess = Some (first_avail.possible |> List.hd) in
        let new_grid_1 = insert_into_grid state.current_grid first_avail.loc first_guess in
        let new_available_list_1 = update_available_list state.available_list first_avail.loc (first_avail.possible |> List.hd) in

        match first_avail.possible |> List.length with
          | 2 ->
            let second_guess = Some (first_avail.possible |>  List.rev |> List.hd) in
            let new_grid_2 = insert_into_grid state.current_grid first_avail.loc second_guess in
            let new_available_list2 = update_available_list state.available_list first_avail.loc (first_avail.possible |>  List.rev |> List.hd) in
    
            Some ({state with current_grid = new_grid_1; available_list = new_available_list_1 |> nonzeroAvailables}, {state with current_grid = new_grid_2; available_list = new_available_list2 |> nonzeroAvailables})
          | _ -> (* V tem primeru moramo en element parametra possible odstraniti. *)
          let new_available_list_2 =
            if List.length state.available_list = 0 then
              assert false
            else
              let update_avail i avail =
                if i = 0 then
                  {avail with possible = first_avail.possible |> List.tl}
                else
                  avail
              in
              List.mapi update_avail state.available_list
            in
          Some ({state with current_grid = new_grid_1; available_list = new_available_list_1 |> nonzeroAvailables}, {state with available_list = new_available_list_2 |> nonzeroAvailables})
        

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
