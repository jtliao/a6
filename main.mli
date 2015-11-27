open Dict
open IOfile

(*Type to represent the different possibilities for constraints in the WHERE
  statement*)
type constraint =
  |Eq of col * wrapper
  |Lt of col * wrapper
  |Gt of col * wrapper

(*Select (col, tab, Some constraint) indicates the command SELECT col FROM tab
  WHERE constraint. *)
type command =
  |Select of string*string*(constraint option)
  |Table of

(*Parses the string that the user inputs into a type command*)
val parse_input : string -> command

(*Acts on the command that is inputted, taking the pair of current
  database data structures, with the first dict being the name-index dict and
  the second being the index-array dict, as parameters and returning the
  updated pair after the command has been executed*)
val execute : command -> dict * dict -> dict * dict

(*The function that will act as the REPL, repreatedly running and passing
  in the current data structures as parameters, with the first dict in the pair
  being the name-index dict and the second being the index-array dict*)
val run_repl : dict * dict -> unit

(*The "main" function that will ask for the database file name and import
  it using iofile.ml, then run the REPL*)
val _ : unit



