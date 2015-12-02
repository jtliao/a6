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
  let new_dict = try (execute (parse_input user_command) dict )
                with |Failure e -> (print_string e; dict)
   in save_tables new_dict dict; run_repl (new_dict)

(*Read the initial table and run the REPL*)
let _ =
  let new_table = print_string "Welcome to the DBMS! You may open an existing
  table by entering the table name (without file extension), or run a
  CREATE TABLE query.\n"; Hashtbl.create 10 in
    run_repl (new_table)