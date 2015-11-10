open Dict
open IOfile

(*Parse the user's input/query and act on it, taking the pair of current
  database data structures, with the first dict being the name-index dict and
  the second being the index-array dict, as parameters and returning the
  updated pair after the input has been executed*)
val parse_input : string -> dict * dict -> dict * dict

(*The function that will act as the REPL, repreatedly running and passing
  in the current data structures as parameters, with the first dict in the pair
  being the name-index dict and the second being the index-array dict*)
val run_repl : dict * dict -> unit

(*The "main" function that will ask for the database file name and import
  it using iofile.ml, then run the REPL*)
val _ : unit



