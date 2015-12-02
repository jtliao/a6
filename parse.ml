open Execute
open Iofile
open Str

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
  else if String.get s 0 = ''' && String.get s ((String.length s)-1) = '''
    then String.sub s 1 ((String.length s) - 2)
  else if String.get s 0 = ''' && String.get s ((String.length s)-1) = ','
    then String.sub s 1 ((String.length s) - 2)
   else if String.get s 0 = ''' && String.get s ((String.length s)-1) = ')'
    then String.sub s 1 ((String.length s) - 2)
  else s
  (*Possibly include another case where the word s is surrounded by apostrophes*)

(*If s is a valid string of an int, then convert it to an int.
NOTE: Use max_int to indicate that the function failed.*)
let decide_int (s: string) : int =
  try(int_of_string s) with
  |Failure "int_of_string" -> max_int

(*If s is a valid string of an int, then convert it to an int.*)
let decide_float (s: string) : float =
  try(float_of_string s) with
  |Failure "float_of_string" -> max_float

(*Returns true if s is a valid string representation of an int,
false otherwise.*)
let is_int (s: string) : bool =
  match decide_int s with
  |k when k != max_int -> true
  |_ -> false

(*Return true if s is a valid string representation of a float,
false otherwise*)
let is_float (s: string) : bool =
  match decide_float s with
  |k when k != max_float -> true
  |_ -> false

(*Returns the sub-list of l starting at index i up to index j inclusive. *)
let sub_list (l : 'a list) (i: int) (j: int) : 'a list =
  let rec helper l i j c =
    match l with
    |[] -> []
    |h::tl -> if c >= i && c <= j then h::(helper tl i j (c+1))
              else helper tl i j (c+1)
  in helper l i j 0

(*Just gets the number of operators in a string list*)
let num_ops (l : string list) : int =
  let ops = ["="; "!="; "<"; ">"; ">="; "<="] in
  let rec helper l c =
    match l with
    |[] -> c
    |h::tl -> if List.mem h ops then  helper tl (c+1)
            else helper tl c
  in helper l 0

(*Takes a string s and converts it to a wrapper type*)
let convert_to_wrapper (s: string) : wrapper =
  if is_int s then Int (decide_int s)
else if is_float s then Float (decide_float s)
else String s

(*Returns the list of column names in an Update command.*)
let columns_list_update (l: string list): string list =
  let rec helper l c acc =
    match l with
    |[] -> acc
    |h::tl -> if c mod 3 = 0 then helper tl (c+1) (h::acc)
              else helper tl (c+1) acc
  in helper l 0 []

(*Gets the new information to be updated in the columns*)
let info_list_update (l: string list): wrapper list =
  let rec helper l c acc =
    match l with
    |[] -> acc
    |h::tl -> if c mod 3 = 2 then helper tl (c+1) ((convert_to_wrapper h)::acc)
              else helper tl (c+1) acc
  in helper l 0 []


(*Generates a constraint from a string list of 3 elements: a string,
an operator and a wrapper. Returns an Op of operator*)
let gen_constraint_op (l: string list) : constr =
  let ops = ["="; "!="; "<"; ">"; ">="; "<="] in
  match get_index ops (List.nth l 1) with (*Need to determine if the third element in the list*)
  |0 -> Op(Eq ((List.nth l 0), convert_to_wrapper (List.nth l 2)))(*is a string or a floar or an int*)
  |1 -> Op(NotEq ((List.nth l 0), convert_to_wrapper (List.nth l 2)))
  |2 -> Op(Lt ((List.nth l 0), convert_to_wrapper (List.nth l 2)))
  |3 -> Op(Gt ((List.nth l 0), convert_to_wrapper (List.nth l 2)))
  |4 -> Op(GtEq ((List.nth l 0), convert_to_wrapper (List.nth l 2)))
  |5 -> Op(LtEq ((List.nth l 0), convert_to_wrapper (List.nth l 2)))
  |_ -> failwith "Incorrect operator."

let rec gen_constraint (l: string list) : constr =
  let c = num_ops l in
  match List.length l with
  |3 -> gen_constraint_op l
  (*|7 -> if List.mem "and" l then gen_constraint_and l
        else if List.mem "or" l then gen_constraint_or l
        else failwith "Probably a typo or something"*)
  |k when (k >= 7 && k = 4*c - 1) ->
              if List.mem "AND" l then
              And(gen_constraint_op (sub_list l 0 2), gen_constraint (sub_list l 4 ((List.length l) - 1)))
              else if List.mem "OR" l then
              Or(gen_constraint_op (sub_list l 0 2), gen_constraint (sub_list l 4 ((List.length l) - 1)))
              else failwith "Probably a typo or something"
  |_ -> failwith "Too few/incorrect constraints"



(*Parses a string list into a select command*)
let parse_select (l: string list) : command =
  let from_index = get_index l "FROM" in
  let lst = sub_list l 1 (from_index - 1) in
  let table = List.nth l (from_index + 1) in
    match get_index l table = (List.length l) - 1 with
    |true -> Select (lst, table, None)
    |false -> let subl = sub_list l ((get_index l table)+2) ((List.length l)-1) in
    Select (lst, table, Some (gen_constraint subl))


(*Just need to add one more helper function to finish this function...*)
let parse_update (l: string list) : command =
  let table = List.nth l 1 in
  let where_index = get_index l "WHERE" in
  let subl1 = sub_list l 3 (where_index - 1) in
  let s = columns_list_update subl1 in
  let w = info_list_update subl1 in
  let subl2 = sub_list l (where_index + 1) ((List.length l) - 1) in
  Update (table, s, w, gen_constraint subl2)


(*Come back to this. The star case might need to be addressed.*)
let parse_delete (l: string list) : command =
  let table = List.nth l 2 in
  let subl = sub_list l 4 ((List.length l) - 1) in
  Delete(table, gen_constraint subl)

let parse_insert (l: string list) : command =
  let table = List.nth l 2 in
  let k = get_index l "VALUES" in
  if k = 3
   then
  Insert(table, [], List.map (fun s -> convert_to_wrapper s) (sub_list l 4 ((List.length l) - 1)))
  else
    let columns = sub_list l 3 (k-1) in
    let str_list = sub_list l (k+1) ((List.length l) - 1) in
    let w_list = List.map (fun s -> convert_to_wrapper s) str_list in
  Insert(table, columns, w_list)

let parse_create (l: string list) : command =
  let table = List.nth l 2 in
    if List.length l = 3 then Create (table, [])
  else
    Create(table, sub_list l 4 ((List.length l) - 1))

let parse_drop (l: string list) : command = Drop (List.nth l 1)

(*Parses the string that the user inputs into a type command*)
let parse_input (s:string) : command =
  let coms = ["SELECT"; "UPDATE"; "DELETE"; "INSERT"; "CREATE"; "DROP"] in
  let regexp = regexp " " in
  let l = split regexp s in
  let mapped = List.map (fun s -> strip s) l in
  match get_index coms (List.nth mapped 0) with
  |0 -> parse_select mapped
  |1 -> parse_update mapped
  |2 -> parse_delete mapped
  |3 -> parse_insert mapped
  |4 -> parse_create mapped
  |5 -> parse_drop mapped
  |_ -> failwith "Typo"