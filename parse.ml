open Execute

(*Gets the first index of a string that appears in a list. Used in parse_input.*)
let get_index (l : 'a list) (s: 'a) : int =
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
let gen_constraint (l: string list) : constr =failwith ""


(*Parses a string list into a select command*)
let parse_select (l: string list) : command =
  let from_index = get_index l "FROM" in
  let lst = sub_list l 1 (from_index - 1) in
  let table = List.nth l (from_index + 1) in
    match get_index l table = (List.length l) - 1 with
    |true -> Select (lst, table, None)
    |false -> let subl = sub_list l ((get_index l table)+2) ((List.length l)-1) in
              Select (lst, table, Some (gen_constraint subl))

(*Parses the string that the user inputs into a type command*)
let parse_input (s:string) : command =failwith""(*
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
  |_ -> failwith "Typo"*)