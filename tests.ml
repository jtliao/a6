open Main
open Iofile
open Assertions

(*type command =
  |Update of string * string list * wrapper list * constr
  |Delete of string * constr
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

---this stuff above is just for reference, will delete later*)

(*****************************UNIT TESTS FOR MAIN*****************************)

(*Tests for update*)
(*1, updating a single element*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"|];Hashtbl.create 3
let updated1 = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Update("People", ["Name"], [String "Jason"],
  Op (Eq ("Name", String "Bob")))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  updated1===new_tab_dict

(*2, updating a single element with multiple constraints*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"; Int 1|];
  Hashtbl.add arr_dict 1 [|String "Bob"; Int 2|]; Hashtbl.create 3
let updated2 = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Update("People", ["Name"; "Age"], [String "Jason"; Int 3],
  And (Op (Eq ("Name", String "Bob")), Op (Eq ("Age", Int 2))))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Bob"; Int 1|];
  Hashtbl.add new_arr_dict 1 [|String "Jason"; Int 3|]; Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  updated2===new_tab_dict

(*3, updating multiple elements*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"; Int 1|];
  Hashtbl.add arr_dict 1 [|String "Bob"; Int 2|]; Hashtbl.create 3
let updated2 = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Update("People", ["Name"; "Age"], [String "Jason"; Int 3],
  (Op (Eq ("Name", String "Bob"))))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"; Int 3|];
  Hashtbl.add new_arr_dict 1 [|String "Jason"; Int 3|]; Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  updated2===new_tab_dict

(*Tests for delete*)


let x = print_string "All tests run.\n"