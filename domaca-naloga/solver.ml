type available = { loc : int * int; possible : int list}
type enumerated_avialable_list = { digit : int; coords_list : (int * int) list}
type state = { problem : Model.problem; current_grid : int option Model.grid; available_list: available list; konec : bool}
let sudoku_numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]

(* let example_available_list = [
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
] *)

let states_table = Hashtbl.create 1000;;
let cached_state (state : state) = 
  Hashtbl.add states_table state (); 
  if (Hashtbl.mem states_table state) then false else true

(* let test_grid = Model.list_to_grid (Model.random_int_list 81);;
let test_option_grid = [|[|None; Some 6; Some 6; Some 2; Some 4; Some 1; Some 2; Some 5; Some 4|];
[|Some 1; Some 2; Some 7; Some 5; Some 7; Some 3; Some 4; Some 7; Some 2|];
[|Some 8; Some 2; Some 7; Some 4; None; Some 7; Some 8; None; Some 2|];
[|Some 2; Some 8; Some 3; Some 3; Some 8; Some 1; Some 6; Some 1; Some 3|];
[|Some 5; Some 6; Some 4; None; Some 4; Some 2; Some 7; Some 8; Some 8|];
[|Some 7; Some 1; Some 5; Some 3; Some 5; None; Some 2; Some 6; Some 3|];
[|Some 4; Some 2; None; Some 3; Some 2; Some 2; Some 7; Some 4; Some 4|];
[|Some 5; Some 8; Some 6; Some 2; None; Some 5; Some 7; Some 3; Some 2|];
[|Some 7; Some 2; Some 4; Some 7; Some 1; Some 3; Some 5; Some 6; None|]|];; *)

let copy_available avail =
  { loc = avail.loc; possible = List.map (fun x -> x) avail.possible }

let copy_available_list available_list =
  List.map copy_available available_list

let print_available_list avail_list =
  List.iter (fun avail ->
    let (x, y) = avail.loc in
    Printf.printf "Location: (%d,%d), Possible: %s\n" x y (String.concat ", " (List.map string_of_int avail.possible))
  ) avail_list

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

(* let test_a_list = test_option_grid |> grid_to_avail_list *)

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

let remove_available_by_loc location available_list =
  List.filter (fun avail -> avail.loc <> location) available_list

let extract_singletons available_list =
  List.filter (fun avail -> List.length avail.possible = 1) available_list
  |> List.map (fun avail -> avail.loc)
let update_and_remove_singletons available_list =
  let singletons = extract_singletons available_list in
  let updated_list = 
    List.fold_left 
      (fun acc_list singleton_loc ->
          let singleton_avail = List.find_opt (fun avl -> avl.loc = singleton_loc) available_list in
          match singleton_avail with
          | Some avl -> 
              let value = match avl.possible with
                | [v] -> v
                | _ -> failwith "Not a singleton"
              in
              List.map (remove_from_possible singleton_loc value) acc_list
          | None -> acc_list
      ) 
      available_list singletons
  in
  List.filter (fun avail -> not (List.mem avail.loc singletons)) updated_list


let update_and_remove_singletons available_list =
  let singletons = singularAvailables available_list in
  
  let rec update sings acc_list = 
    match sings with
    | [] -> acc_list
    | x :: xs -> 
        (match x.possible with 
          | [v] -> 
              let updated_list = update_available_list acc_list x.loc v in
              update xs updated_list
          | _ -> failwith "This shouldn't happen")

  in update singletons available_list

(* let update_and_remove_singletons available_list =
  let singletons = singularAvailables available_list in
  
  let rec update sings = 
    match sings with
    | [] -> available_list
    | x :: xs -> 
      (match x.possible with 
      |[v] -> let value = v in let loc = x.loc in
      update_available_list available_list loc value
      | _ -> failwith "ne more se zgoditi")

    in update singletons *)

  (* let update_and_extract_singletons available_list =
    let singletons = List.filter (fun avail -> List.length avail.possible = 1) available_list in
    let singletons_data = List.map (fun avail -> (avail.loc, List.hd avail.possible)) singletons in
    let updated_list = 
      List.fold_left 
        (fun acc_list (singleton_loc, value) ->
          List.map (remove_from_possible singleton_loc value) acc_list
        ) 
        available_list singletons_data
    in
    (List.filter (fun avail -> not (List.mem_assoc avail.loc singletons_data)) updated_list, singletons_data)
  
  (* Function to insert singletons into a given grid *)
  let insert_singletons_into_grid grid singletons_data =
    List.fold_left (fun current_grid (loc, value) -> insert_into_grid current_grid loc (Some value)) grid singletons_data *)

(* Morda lahko nekoliko razširim trivialno reševanje... *)
(* let solve_trivial_cells grid available_list=
  let singular_availables = available_list |> singularAvailables in
  if singular_availables = [] then grid 
  else
  List.hd (List.map (fun avail ->  insert_into_grid grid avail.loc  (Some (List.hd avail.possible))) singular_availables) *)
   
  let solve_trivial_cells grid available_list =
    let singular_availables = singularAvailables available_list in
    if singular_availables = [] then grid
    else
        List.fold_left (fun current_grid avail -> 
            insert_into_grid current_grid avail.loc (Some (List.hd avail.possible))
        ) grid singular_availables

  (* Vzamemo nek grid, ki ga dobimo iz state.current_grid, in mu zapolnimo trivialne celice.*)

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "-" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; problem; available_list = problem.initial_grid |> grid_to_avail_list |> nonzeroAvailables; konec = false}

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

  if state.konec then None else

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
      (* print_endline "Originalni list -------------------------------------";
      state.available_list |> print_available_list;
      print_endline "Originalna to avail list -------------------------------------";
      state.current_grid |> grid_to_avail_list |> print_available_list; *)
      (* print_endline "Stara verzija -------------------------------------";
      trivially_corrected |> grid_to_avail_list  |> print_available_list;
      print_endline "Nova verzija -------------------------------------";
      state.available_list |> update_and_remove_singletons |> nonzeroAvailables |> print_available_list;
      print_endline "---------------------------------------------------"; *)
      (* TODO Tukaj je treba odstraniti grid_to_avail_list *)
      Some ({state with current_grid = trivially_corrected; available_list = state.available_list |> update_and_remove_singletons |> nonzeroAvailables }, {state with available_list = []})
    else
        let smallest_avail = state.available_list |> findSmallestAvailable in
        let first_guess = Some (smallest_avail.possible |> List.hd) in
        let new_grid_1 = insert_into_grid state.current_grid smallest_avail.loc first_guess in
        let new_available_list_1 = remove_available_by_loc smallest_avail.loc (update_available_list state.available_list smallest_avail.loc (smallest_avail.possible |> List.hd))  in
        (* print_endline "tukaj napaka?"; *)
        (* new_available_list_1 |> print_available_list;
        print_endline "---------------------------------------------------";
        new_grid_1 |> grid_to_avail_list |> print_available_list;
        print_endline "---------------------------------------------------"; *)
        match smallest_avail.possible |> List.length with
          | 2 ->
            let second_guess = Some (smallest_avail.possible |> List.rev |> List.hd) in
            let new_grid_2 = insert_into_grid state.current_grid smallest_avail.loc second_guess in
            let new_available_list_2 = remove_available_by_loc smallest_avail.loc (update_available_list state.available_list smallest_avail.loc (smallest_avail.possible |>  List.rev |> List.hd)) in
            (* new_available_list_2 |> print_available_list;
            print_endline "---------------------------------------------------";
            new_grid_2 |> grid_to_avail_list |> print_available_list;
            print_endline "---------------------------------------------------"; *)
            Some ({state with current_grid = new_grid_1; available_list = new_available_list_1 |> nonzeroAvailables}, {state with current_grid = new_grid_2; available_list = new_available_list_2 |> nonzeroAvailables})
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
