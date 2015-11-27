open IOfile

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

let fst = function
|(f, _, _) -> f

let snd = function
|(_, s, _) -> s

let trd = function
|(_, _, t) -> t

(*Parses the string that the user inputs into a type command*)
let parse_input (s:string) : command =
  failwith "TODO"

let rec get_col_indices (cols:string list) (d: dict) : int list =
  match cols with
  |[] -> []
  |h::t -> (match Dict.lookup dict h with
            |Some x -> x::(get_col_indices t d)
            |None -> get_col_indices t d)

let execute_select (cols : string list) (tab: string) (cons: constraint)
(dicts: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t) : unit =
  let table_dicts = match Dict.lookup (fst dicts) tab with
                    |Some x -> x
                    |None -> failwith "doesnt exist" in
  let index_list = get_col_indices cols (snd dicts) in

let execute_update =failwith "TODO"

let execute_delete =failwith "TODO"

let execute_insert =failwith "TODO"

let execute_create =failwith "TODO"

let execute (com:command) (dicts: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t *
  ('e*'f) Hashtbl.t):('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t =
  match com with
  |Select (col, tab, None) -> execute_select col tab dicts; dicts
  |Select (col, tab, Some cons) -> execute_select col tab dicts; dicts
  |Update () -> execute_update
  |Delete () -> execute_delete
  |Insert () -> execute_insert
  |Create () -> execute_create
  |Drop t -> (remove t (fst dicts), snd dicts, trd dicts)

let rec run_repl (dicts: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t *
  ('e*'f) Hashtbl.t) : unit =
  let user_command = read_line () in
  run_repl (execute (parse_input user_command) dicts)

let _ =
  let file_name = print_string ("Enter the name of the file containing
    the database:\n"); read_line () in
  run_repl (string_to_dict (readfile file_name))