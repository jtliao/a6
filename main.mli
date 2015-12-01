open Iofile
open Execute
open Parse

(*The function that will act as the REPL, repeatedly running and passing
  in the hashtable as a parameter, with the hashtable mapping
  the table name to the table's values *)
val run_repl : (string, (string, int) Hashtbl.t *
  (int, wrapper array) Hashtbl.t) Hashtbl.t -> unit