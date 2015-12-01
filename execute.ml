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
  |And of constr * constr
  |Or of constr * constr

type command =
  |Select of string list * string * constr option
  |Update of string * string list * wrapper list * constr
  |Delete of string * constr
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

(*Gets the first index of a string that appears in a list. Used in parse_input.*)
let get_index (l : 'a list) (s: 'a) : int =
  let rec helper l s c =
    match l with
    |[] -> failwith "Empty list"
    |h::tl -> if h = s then c else helper tl s (c+1)
  in helper l s 0

(*Return the list of indices of inputtedcolumns in the table*)
let rec get_col_indices (cols : string list) (col_dict : ('a,'b) Hashtbl.t)
: int list =
  match cols with
  |[] -> []
  |h::t -> (Hashtbl.find col_dict h)::(get_col_indices t col_dict)

(*Return a list of all ints from 0 to [len]*)
let rec all_col_indices (ind : int) (len : int): int list =
  if ind >= len then []
  else ind::(all_col_indices (ind+1) len)

(*Evaluate if the inputted operator is true or false*)
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

(*Evaluate if the inputted constraint is true or false*)
let rec eval_constraint (arr: wrapper array) (cons: constr)
(col_dict: ('a,'b) Hashtbl.t) : bool=
  match cons with
  |Op o1 -> eval_operator arr o1 col_dict
  |And (c1, c2) -> (eval_constraint arr c1 col_dict) &&
                   (eval_constraint arr c2 col_dict)
  |Or (c3, c4)-> (eval_constraint arr c3 col_dict) ||
                  (eval_constraint arr c4 col_dict)

(*Constructs the string with the elements of an array*)
let rec get_elements (arr: wrapper array) (cols : int list) (str : string ref) : unit =
  match cols with
  |[] -> ()
  |h::t -> (str:= !str ^"        "^ wrap_to_string arr.(h); get_elements arr t str)

(*Print the string with the elements queried by the select command on [tab]*)
let execute_select (cols : string list) (tab: string) (cons: constr option)
(dict: ('a,'b) Hashtbl.t) : unit =
  try (
    let table_dicts = Hashtbl.find dict tab in
    let index_list = if cols = ["*"]
                     then all_col_indices 0 (Hashtbl.length (fst table_dicts))
                     else get_col_indices cols (fst table_dicts) in
    let select_string = ref "" in
    (*add column headers to string here*)
    for x = 0 to Hashtbl.length (snd table_dicts) do
      let arr = Hashtbl.find (snd table_dicts) x in
      match cons with
      |None -> get_elements arr index_list select_string
      |Some c -> if eval_constraint arr c (fst table_dicts)
                 then
                   (get_elements arr index_list select_string;
                    select_string:= !select_string^"\n")
                 else ()
    done; print_string !select_string
  )
  with
    |Not_found  -> print_string "Invalid constraint query."

(*Update a single array with specified values in the specified columns*)
let rec update_array (arr: wrapper array) (cols : int list) (orig : int list)
(vals : wrapper list) (cons: constr) : unit =
  match cols with
  |[] -> ()
  |h::t -> let ind = get_index orig h in
             (arr.(h) <- List.nth vals ind); update_array arr t orig vals cons

(*Execute the update command on [tab] and return the updated hashtable*)
let execute_update (tab : string) (cols : string list) (vals: wrapper list)
(cons : constr) (dict : ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  let table_dicts = Hashtbl.find dict tab in
  let index_list = get_col_indices cols (fst table_dicts) in
  for x = 0 to Hashtbl.length (snd table_dicts)-1 do
    let arr = Hashtbl.find (snd table_dicts) x in
    if eval_constraint arr cons (fst table_dicts)
    then
      update_array arr index_list index_list vals cons
    else ()
  done; dict

(*Execute the delete command on [tab] and return the updated hashtable*)
let execute_delete (tab : string) (cons : constr)
(dict : ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  let table_dicts = Hashtbl.find dict tab in
  let cntr = ref 0 in
  let size = ref (Hashtbl.length (snd table_dicts)) in
  while !cntr <= !size-1 do
    let arr = Hashtbl.find (snd table_dicts) !cntr in
    if eval_constraint arr cons (fst table_dicts)
    then
      if !cntr <= !size-2 then
        (for i = !cntr to !size-2 do
          Hashtbl.add (snd table_dicts) i (Hashtbl.find (snd table_dicts) (i+1))
        done; Hashtbl.remove (snd table_dicts) (!size-1); size:= !size -1)
      else ()
    else incr cntr
  done; dict

(*Execute the insert command on [tab] and return the updated hashtable*)
let execute_insert (tab: string) (cols : string list) (wrap: wrapper list)
(dict: ('a,'b) Hashtbl.t) : ('a,'b) Hashtbl.t =
  try (
    let table_dicts = Hashtbl.find dict tab in
    let col_indices = get_col_indices cols (fst table_dicts) in
    let arr_length = Hashtbl.length (fst table_dicts) in
    let arr = Array.make arr_length Null in
    for x = 0 to (arr_length-1) do
      if List.exists (fun n -> n=x) col_indices
      then
        let ind = get_index col_indices x in
        arr.(x) <- (List.nth wrap ind)
      else ()
    done; Hashtbl.add (snd table_dicts) (Hashtbl.length (snd table_dicts)) arr;
      Hashtbl.add dict tab (fst table_dicts, snd table_dicts); dict
  )
  with
    |_ -> print_string "Invalid INSERT query."; dict

(*Create a table with specified columns in them*)
let rec create_col_dict (cols : string list) (col_dict: ('a,'b) Hashtbl.t)
(index : int): ('a,'b) Hashtbl.t=
  match cols with
  |[] -> col_dict
  |h::t -> Hashtbl.add col_dict h index; create_col_dict t col_dict (index+1)

(*Execute the create command on [tab] and return the updated hashtable*)
let execute_create (tab: string) (col : string list) (dict: ('a,'b) Hashtbl.t)
: ('a,'b) Hashtbl.t =
    Hashtbl.add dict tab (create_col_dict col
      (Hashtbl.create (List.length col)) 0, Hashtbl.create 20); dict

(*Check the validity of column names*)
let rec check_cols (col : string list) (dict : ('a,'b) Hashtbl.t) : bool =
  match col with
  |[] -> true
  |h::t -> (Hashtbl.mem dict h) && (check_cols t dict)

(*Check the validity of a command*)
let check_command (col : string list) (tab : string)
(dict : ('a,'b) Hashtbl.t) : bool =
  if Hashtbl.mem dict tab
  then
    let table_dict = Hashtbl.find dict tab in
    if check_cols col (fst table_dict)
    then true
    else (print_string "Column(s) do(es) not exist in this table."; false)
  else (print_string "Table does not exist."; false)

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
                         else (print_string "Table does not exist."; dict)
  |Insert (tab, col, wrap) -> if check_command col tab dict
                              then execute_insert tab col wrap dict
                              else dict
  |Create (tab, col) -> if Hashtbl.mem dict tab
                        then (print_string "Table already exists,
                           please choose a different name"; dict)
                        else execute_create tab col dict
  |Drop tab -> if Hashtbl.mem dict tab
               then (Hashtbl.remove dict tab; dict)
               else (print_string "Table does not exist"; dict)