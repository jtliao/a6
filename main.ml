open Iofile
open Execute
open Parse

(*After executing a command, save tables that have been changed*)
let save_tables (new_dict: ('a,'b) Hashtbl.t) (dict: ('a,'b) Hashtbl.t): unit =
  let f str tabs = (*if tabs = Hashtbl.find dict str then ()
                   else*) write_file (dict_to_string tabs) (str^".txt") in
  Hashtbl.iter f new_dict

let rec run_repl (dict: ('a,'b) Hashtbl.t) : unit =
  let user_command = read_line () in
  let new_dict = execute (parse_input user_command) dict in
  save_tables new_dict dict ; run_repl (new_dict)

(*Read the initial table and run the REPL*)
let _ =
  let new_table = print_string "Welcome to the DBMS! You may open an existing
  table by entering the table name (without file extension), or run a
  CREATE TABLE query.\n"; Hashtbl.create 10 in
    run_repl (new_table)

(*(*Read the initial table and run the REPL*)
let _ =
  let table_name = print_string ("Enter the name of existing table to open
    (without file extension), or run a CREATE TABLE command:\n");read_line () in
  let file_name = table_name^".txt" in
  let new_table = Hashtbl.create 10 in
  let new_table2 = Hashtbl.create 10 in
  let new_table3 = Hashtbl.create 15 in
  let dict = try (string_to_dict (read_file file_name))
             with _ -> (new_table2, new_table3) in
  Hashtbl.add new_table table_name (dict);
    run_repl (new_table)*)