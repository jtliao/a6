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

let eval_operator (arr:wrapper array) (op: operator) (dicts) : bool option=
    match op with
    |Eq (c1, w1) -> Some (arr.(Hashtbl.find (snd dicts) c1) = w1)
    |Lt (c2, w2) -> Some (arr.(Hashtbl.find (snd dicts) c2) < w2)
    |Gt (c3, w3) -> Some (arr.(Hashtbl.find (snd dicts) c3) > w3)
    |LtEq (c4, w4) -> Some (arr.(Hashtbl.find (snd dicts) c4) <= w4)
    |GtEq (c5, w5) -> Some (arr.(Hashtbl.find (snd dicts) c5) >= w5)
    |NotEq (c6, w6) -> Some (arr.(Hashtbl.find (snd dicts) c6) <> w6)

let rec eval_constraint (arr: wrapper array) (cons: constraint) (dicts): bool=
  match cons with
  |Op o1 -> eval_operator arr op1 dicts
  |And (o2, c1) -> (eval_operator arr op2 dicts)&&(eval_constraint arr c1 dicts)
  |Or (op3, c2)-> (eval_operator arr op3 dicts)||(eval_constraint arr c2 dicts)

let rec get_elements (arr: wrapper array) (cols : int list) (str : string ref)
(cons: constraint) (dicts): unit =
  match cons, cols with
  |None, [] -> ()
  |None, h::t -> str:= !str ^"        "^ arr.(h);
                 get_elements arr t str cons dicts
  |Some c, [] -> ()
  |Some c, h::t -> if eval_constraint arr then str:= !str ^"        "^ arr.(h);
                      get_elements arr t str cons dicts
                   else get_elements arr t str cons dicts

let execute_select (cols : string list) (tab: string) (cons: constraint)
(dicts: ('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t) : unit =
  try (
    let table_dicts = Hashtbl.find (fst dicts) tab in
    let index_list = get_col_indices cols (snd dicts) in
    let selected_string = ref "" in
    (*add column headers to string here*)
    for x = 0 to Hashtbl.length (trd dicts) do
      let arr = Hashtbl.find (trd dicts) x in
      get_elements arr cols selected_string cons dicts
    done
  )
  with
    |Exception e -> "Invalid query, try again."; ()


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