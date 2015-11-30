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

(*Gets the first index of a string that appears in a list. Used in parse_input.*)
let get_index (l : string list) (s: string) : int =
  let rec helper l s c =
    match l with
    |[] -> failwith "Empty list"
    |h::tl -> if h = s then c else helper tl s (c+1)
  in helper l s 0

(*Takes a string and strips it of leading/trailing parentheses
*and commas. Used for formatting the commands.*)
let strip (s:string) : string =
  if String.get s 0 = '(' && String.get s ((String.length s) - 1) = ','
    then String.sub s 1 ((String.length s) - 2)
  else if String.get s ((String.length s) - 1) = ','
    then String.sub s 0 ((String.length s) - 1)
  else if String.get s ((String.length s) - 1) = ')'
    then String.sub s 0 ((String.length s) - 1)
  else s
  (*Possibly include another case where the word s is surrounded by apostrophes*)



(*Returns the sub-list of l starting at index i up to index j inclusive. *)
let sub_list (l : 'a list) (i: int) (j: int) : 'a list =
  let rec helper l i j c =
    match l with
    |[] -> []
    |h::tl -> if c >= i && c <= j then h::(helper tl i j (c+1))
              else helper tl i j (c+1)
  in helper l i j 0

(*Generates a constraint from a string list.*)
let gen_constraint (l: string list) : command =


(*Parses a string list into a select command*)
let parse_select (l: string list) : command =
  let from_index = get_index l "FROM" in
  let lst = sub_list l 1 (from_index - 1) in
  let table = List.nth l (from_index + 1) in
    match get_index l table = (List.length l) - 1 with
    |true -> Select (lst, table, None)
    |false -> let subl = sub_list l ((get_index l table)+2) ((List.length l)-1) in
    Select (lst, table, gen_constraint subl)

(*Parses the string that the user inputs into a type command*)
let parse_input (s:string) : command =
  let coms = ["SELECT"; "UPDATE"; "DELETE"; "INSERT"; "CREATE"; "DROP"] in
  let regexp = Str.regexp " " in
  let l = Str.split regexp s in
  let mapped = List.map (fun s -> strip s) l in
  match get_index coms (List.nth l 0) with
  |0 -> parse_select mapped
  |1 -> parse_update mapped
  |2 -> parse_delete mapped
  |3 -> parse_insert mapped
  |4 -> parse_create mapped
  |5 -> parse_drop mapped
  |_ -> failwith "Typo"


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