(* open ExtLib
 *)
type wrapper =
  |Int of int
  |Float of float
  |String of string
  |Null

(*Convert a wrapped value into a string for printing *)
val wrap_to_string : wrapper -> string

(*Takes the name of the file to read and then reads it into a string*)
val read_file : string -> string

(*Takes the string to be written and the name of the file to write to, then
  writes to the file and saves it*)
val write_file : string -> string-> unit

(*Takes the string that is read from database and converts it into our own
  dictionaries.
  The first dict in the pair is the name-index dict, and the second is the
  index-array dict.*)
val string_to_dict : string -> (string, int) Hashtbl.t *
  (int, wrapper array) Hashtbl.t

(*Takes in a dict pair and then prints back to the database form so that it can
  be printed back to the file.
  The first dict in the pair is the name-index dict, and the second is the
  index-array dict.*)
val dict_to_string : (string, int) Hashtbl.t *
  (int, wrapper array) Hashtbl.t -> string