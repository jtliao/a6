open Iofile

type operator =
  |Eq of string * wrapper
  |Lt of string * wrapper
  |Gt of string * wrapper
  |LtEq of string * wrapper
  |GtEq of string * wrapper
  |NotEq of string * wrapper

type constr =
  |Op of operator
  |And of operator * constr
  |Or of operator * constr

type command =
  |Select of string list * string * constr option
  |Update of string * string list * string list * constr
  |Delete of string * constr
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

(*let rec get_index (val : 'a) (lst: 'a list) : int =
  match lst with
  |[] -> raise Not_found
  |h::t -> if val = h then 0 else 1 + (get_index val t)*)

(*Parses the string that the user inputs into a type command*)
let parse_input (s:string) : command =
  failwith "TODO"

let rec get_col_indices (cols : string list) (dict : ('a,'b) Hashtbl.t)
: int list =
  match cols with
  |[] -> []
  |h::t -> (Hashtbl.find dict h)::(get_col_indices t dict)

let eval_operator (arr : wrapper array) (op : operator)
(col_dict : ('a,'b) Hashtbl.t) : bool=
  try (
    match op with
    |Eq (c1, w1) -> arr.(Hashtbl.find col_dict c1) = w1
    |Lt (c2, w2) -> arr.(Hashtbl.find col_dict c2) < w2
    |Gt (c3, w3) -> arr.(Hashtbl.find col_dict c3) > w3
    |LtEq (c4, w4) -> arr.(Hashtbl.find col_dict c4) <= w4
    |GtEq (c5, w5) -> arr.(Hashtbl.find col_dict c5) >= w5
    |NotEq (c6, w6) -> arr.(Hashtbl.find col_dict c6) <> w6
  )
  with
  |Not_found -> raise Not_found

let rec eval_constraint (arr: wrapper array) (cons: constr)
(col_dict: ('a,'b) Hashtbl.t) : bool=
  match cons with
  |Op o1 -> eval_operator arr o1 col_dict
  |And (o2, c1) -> (eval_operator arr o2 col_dict) &&
                   (eval_constraint arr c1 col_dict)
  |Or (o3, c2)-> (eval_operator arr o3 col_dict) ||
                  (eval_constraint arr c2 col_dict)

let rec get_elements (arr: wrapper array) (cols : int list) (str : string ref) : unit =
  match cols with
  |[] -> ()
  |h::t -> (str:= !str ^"        "^ get_wrapped arr.(h)); get_elements arr t str cons

let execute_select (cols : string list) (tab: string) (cons: constr option)
(dict: ('a,'b) Hashtbl.t) : unit =
  try (
    let table_dicts = Hashtbl.find dict tab in
    let index_list = get_col_indices cols (fst table_dicts) in
    let selected_string = ref "" in
    (*add column headers to string here*)
    for x = 0 to Hashtbl.length (snd table_dicts) do
      let arr = Hashtbl.find (snd table_dicts) x in
      match cons with
      |None -> get_elements arr index_list selected_string
      |Some c -> if eval_constraint arr c (fst table_dicts)
                 then
                   get_elements arr index_list selected_string
                 else ()
    done; print_string !selected_string
  )
  with
    |Not_found e -> print_string "Invalid constraint query."

let rec update_array (arr: wrapper array) (cols : int list) (orig : int list)
(vals : wrapper list) (cons: constr) : unit =
  match cols with
  |[] -> ()
  |h::t -> let ind = get_index h orig in
             (arr.(h) <- List.nth vals ind); update_array arr t orig vals cons

let execute_update (tab : string) (cols : string list) (vals: wrapper list)
(cons : constr) (dict : ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  let table_dicts = Hashtbl.find dict tab in
  let index_list = get_col_indices cols (fst table_dicts) in
  (for x = 0 to Hashtbl.length (snd table_dicts) do
    let arr = Hashtbl.find (snd table_dicts) x in
    if eval_constraint arr cons (fst table_dicts)
    then
      update_array arr index_list index_list vals cons
    else ()
  done); dict

let delete_array =
  sdf

let execute_delete (tab : string) (cons : constr)
(dict : ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  let table_dicts = Hashtbl.find dict tab in
  let index_list = get_col_indices cols (fst table_dicts) in
  (for x = 0 to Hashtbl.length (snd table_dicts) do
    let arr = Hashtbl.find (snd table_dicts) x in
    if eval_constr arr cons (fst table_dicts)
    then
      delete_array
    else ()
  done); dict

let execute_insert (tab: string) (cols : string list) (wrap: wrapper list)
(dict: ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  try (
    let table_dicts = Hashtbl.find dict tab in
    let col_indices = get_col_indices cols (fst table_dicts) in
    let arr_length = Hashtbl.length (fst table_dicts) in
    let arr = Array.make arr_length Null in
    for x = 0 to (arr_length-1) do
      if List.exists (fun n -> n=x) col_indices
      then let ind = get_index x cols in
        arr.(x) <- (List.nth wrap ind)
      else ()
    done
      Hashtbl.add (snd table_dicts) (Hashtbl.length (snd table_dicts)) arr;
      Hashtbl.add dict tab (fst table_dicts, snd table_dicts); dict
  )
  with
    |Exception e -> print_string "Invalid INSERT query."; dict

let rec create_col_dict (cols : string list) (col_dict: ('a,'b) Hashtbl.t)
(index : int): ('a,'b) Hashtbl.t=
  match cols with
  |[] -> col_dict
  |h::t -> Hashtbl.add col_dict h index; create_col_dict t col_dict (index+1)

let execute_create (tab: string) (col : string list) (dict: ('a,'b) Hashtbl.t)
: ('a,'b) Hashtbl.t =
    Hashtbl.add dict tab (create_col_dict col
      (Hashtbl.create (List.length col)) 0, Hashtbl.create 20); dict

let rec check_cols (col : string list) (dict : ('a,'b) Hashtbl.t) : bool =
  match col with
  |[] -> true
  |h::t -> (Hashtbl.mem dict h) && (check_cols t dict)

let check_command (col : string list) (tab : string)
(dict : ('a,'b) Hashtbl.t) : bool =
  if Hashtbl.mem dict tab
  then
    let table_dict = Hashtbl.find dict tab in
    if check_cols col (fst table_dict)
    then true
    else print_string "Column(s) do(es) not exist in this table."; false
  else print_string "Table does not exist."; false

let execute (com:command) (dict: ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  match com with
  |Select (col, tab, cons) -> if check_command col tab dict
                              then (execute_select col tab cons dict; dict)
                              else dict
  |Update (tab, col, vals, cons) -> if check_command col tab dict
                              then execute_update tab col vals cons dict
                              else dict
  |Delete (tab, cons) -> if Hashtbl.mem dict tab
                         then execute_delete tab cons dict
                         else "Table does not exist."; dict
  |Insert (tab, col, wrap) -> if check_command col tab dict
                              then execute_insert tab col wrap dict
                              else dict
  |Create (tab, col) -> if Hashtbl.mem dict tab
                        then (print_string "Table already exists,
                           please choose a different name"; dict)
                        else execute_create tab col dict
  |Drop tab -> if Hashtbl.mem dict tab
               then (Hashtbl.remove tab dict; dict)
               else (print_string "Table does not exist"; dict)

let rec run_repl (dict: ('a,'b) Hashtbl.t) : unit =
  let user_command = read_line () in
  run_repl (execute (parse_input user_command) dict)

let _ =
  let file_name = print_string ("Enter the name of the file containing
    the database:\n"); read_line () in
  run_repl (string_to_dict (readfile file_name))