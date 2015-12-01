open Iofile
open Execute
open Parse

let rec run_repl (dict: ('a,'b) Hashtbl.t) : unit =
  let user_command = read_line () in
  run_repl (execute (parse_input user_command) dict)

(*Read the initial table and run the REPL*)
let _ =
  let file_name = print_string ("Enter the name of the file with"^
    " the table:\n"); read_line () in
  run_repl (string_to_dict (read_file file_name))