open Iofile

type operator =
  |Eq of string * wrapper
  |Lt of string * wrapper
  |Gt of string * wrapper
  |LtEq of string * wrapper
  |GtEq of string * wrapper
  |NotEq of string * wrapper

type constraint =
  |Op of operator
  |And of operator * constraint
  |Or of operator * constraint

type command =
  |Select of string list * string * constraint option
  |Update of string * string list* constraint
  |Delete of string * constraint
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

let frst = function
|(f, _, _) -> f

let scnd = function
|(_, s, _) -> s

let thrd = function
|(_, _, t) -> t

let rec get_index (val: 'a) (lst: 'a list) : int =
  match lst with
  |[] -> raise Not_found
  |h::t -> if val=h then 0 else 1+(find x t)

(*Parses the string that the user inputs into a type command*)
let parse_input (s:string) : command =
  failwith "TODO"

let rec get_col_indices (cols:string list) (d: dict) : int list =
  match cols with
  |[] -> []
  |h::t -> (Hashtbl.find dict h)::(get_col_indices t d)

let eval_operator (arr:wrapper array) (op: operator) (dicts) : bool=
  match op with
  |Eq (c1, w1) -> arr.(Hashtbl.find dict c1) = w1
  |Lt (c2, w2) -> arr.(Hashtbl.find dict c2) < w2
  |Gt (c3, w3) -> arr.(Hashtbl.find dict c3) > w3
  |LtEq (c4, w4) -> arr.(Hashtbl.find dict c4) <= w4
  |GtEq (c5, w5) -> arr.(Hashtbl.find dict c5) >= w5
  |NotEq (c6, w6) -> arr.(Hashtbl.find dict c6) <> w6

let rec eval_constraint (arr: wrapper array) (cons: constraint) (dict) : bool=
  match cons with
  |Op o1 -> eval_operator arr op1 dict
  |And (o2, c1) -> (eval_operator arr op2 dict)&&(eval_constraint arr c1 dict)
  |Or (op3, c2)-> (eval_operator arr op3 dict)||(eval_constraint arr c2 dict)

let rec get_elements (arr: wrapper array) (cols : int list) (str : string ref)
(cons: constraint) (dict): unit =
  match cons, cols with
  |None, [] -> ()
  |None, h::t -> str:= !str ^"        "^ arr.(h);
                 get_elements arr t str cons dict
  |Some c, [] -> ()
  |Some c, h::t -> if eval_constraint arr then str:= !str ^"        "^ arr.(h);
                      get_elements arr t str cons dict
                   else get_elements arr t str cons dict

let execute_select (cols : string list) (tab: string) (cons: constraint)
(dicts) : unit =
  try (
    let table_dicts = Hashtbl.find (frst dicts) tab in
    let index_list = get_col_indices cols (fst table_dicts) in
    let selected_string = ref "" in
    (*add column headers to string here*)
    for x = 0 to Hashtbl.length (snd table_dicts) do
      let arr = Hashtbl.find (snd table_dicts) x in
      get_elements arr index_list selected_string cons (fst table_dicts)
    done
  )
  with
    |Exception e -> print_string "Invalid SELECT query."


let execute_update =failwith "TODO"

let execute_delete =failwith "TODO"

let execute_insert (tab: string) (cols : string list) (wrap: wrapper list) dicts
: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t =
  try (
    let table_dicts = Hashtbl.find (frst dicts) tab in
    let col_indices = get_col_indices cols (fst table_dicts) in
    let arr_length = Hashtbl.length (fst table_dicts) in
    let arr = Array.make arr_length Null in
    for x = 0 to (arr_length-1) do
      if List.exists (fun n -> n=x) col_indices
      then let ind = get_index x cols in
        arr.(x) <- (List.nth wrap ind)
      else ()
    done
    let new_table_vals = Hashtbl.add (snd table_dicts)
      (Hashtbl.length (snd table_dicts)) arr in
    let new_tables = Hashtbl.add (frst dicts) tab
      (fst table_dicts, new_table_vals) in
    (new_tables, scnd dicts, thrd dicts)
  )
  with
    |Exception e -> print_string "Invalid INSERT query."; dicts

let rec create_col_dict (cols : string list) dict (index : int): ('a*'b) Hashtbl.t=
  match cols with
  |[] -> dict
  |h::t -> Hashtbl.add dict h index; create_col_dict t dict (index+1)

let execute_create (tab: string) (col : string list) dicts
: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t =
  try (
    let table_dict = Hashtbl.add (frst dicts) tab (create_col_dict col
      (Hashtbl.create (List.length col)) 0, Hashtbl.create 20) in
    (table_dict, scnd dicts, thrd dicts)
  )
  with
    |Exception e -> print_string "Invalid CREATE query"; dicts

let execute (com:command) (dicts: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t *
  ('e*'f) Hashtbl.t):('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t =
  match com with
  |Select (col, tab, None) -> execute_select col tab dicts; dicts
  |Select (col, tab, Some cons) -> execute_select col tab dicts; dicts
  |Update () -> execute_update
  |Delete () -> execute_delete
  |Insert (tab, col, wrap) -> execute_insert tab col wrap dicts
  |Create (tab, col) -> if Hashtbl.mem (frst dicts) tab then print_string
                          "Table already exists,
                           please choose a different name"; dicts
                        else execute_create tab col dicts
  |Drop tab -> if Hashtbl.mem (frst dicts) tab then (Hashtbl. remove tab (frst dicts), scnd dicts, thrd dicts)
             else print_string "Table does not exist"; dicts

let rec run_repl (dicts: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t *
  ('e*'f) Hashtbl.t) : unit =
  let user_command = read_line () in
  run_repl (execute (parse_input user_command) dicts)

let _ =
  let file_name = print_string ("Enter the name of the file containing
    the database:\n"); read_line () in
  run_repl (string_to_dict (readfile file_name))