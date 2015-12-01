(* open ExtLib
 *)
type wrapper =
  |Int of int
  |Float of float
  |String of string
  |Null

let wrap_to_string w =
  match w with
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | String s -> s
    | Null -> "null"

let read_file filename =
  let chan = open_in filename in


let write_file changes filename =
  failwith "TODO"

let string_to_dict s =
  failwith "TODO"

let dict_to_string d =
  failwith "TODO"