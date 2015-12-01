open Iofile
open Execute
open Parse

let rec run_repl (dict: ('a,'b) Hashtbl.t) : unit =
  let user_command = read_line () in
  run_repl (execute (parse_input user_command) dict)

(*Read the initial table and run the REPL*)
let _ =
  let table_name = print_string ("Enter the name of existing table to open,
    or name of new table to create (without file extension):\n");read_line () in
  let file_name = table_name^".txt" in
  let new_table = Hashtbl.create 10 in
  let new_table2 = Hashtbl.create 10 in
  let new_table3 = Hashtbl.create 15 in
  let dict = try (string_to_dict (read_file file_name))
             with _ -> (new_table2, new_table3) in
  Hashtbl.add new_table table_name (dict);
    run_repl (new_table)