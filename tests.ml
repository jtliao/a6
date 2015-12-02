open Execute
open Iofile
open Assertions

(*type command =
  |Select of string list * string * constr option
  |Update of string * string list * wrapper list * constr
  |Delete of string * constr
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

---this stuff above is just for reference, will delete later*)

(*****************************UNIT TESTS FOR MAIN*****************************)

(*Tests for update*)
(*#1, updating a single element*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"|];Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Update("People", ["Name"], [String "Jason"],
  Op (Eq ("Name", String "Bob")))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  updated===new_tab_dict

(*#2, updating a single element with multiple constraints*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"; Int 1|];
  Hashtbl.add arr_dict 1 [|String "Bob"; Int 2|]; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Update("People", ["Name"; "Age"], [String "Jason"; Int 3],
  And (Op (Eq ("Name", String "Bob")), Op (Eq ("Age", Int 2))))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Bob"; Int 1|];
  Hashtbl.add new_arr_dict 1 [|String "Jason"; Int 3|]; Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  updated===new_tab_dict

(*#3, updating multiple elements*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"; Int 1|];
  Hashtbl.add arr_dict 1 [|String "Bob"; Int 2|]; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Update("People", ["Name"; "Age"], [String "Jason"; Int 3],
  (Op (Eq ("Name", String "Bob"))))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"; Int 3|];
  Hashtbl.add new_arr_dict 1 [|String "Jason"; Int 3|]; Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  updated===new_tab_dict

(*Tests for create*)
(*#1, creating one table*)
let tab_dict = Hashtbl.create 3
let updated = execute (Create ("People", ["Names"])) tab_dict
let new_col_dict = Hashtbl.create 3
let new_arr_dict = Hashtbl.add new_col_dict "Names" 0; Hashtbl.create 10

TEST_UNIT = dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (new_col_dict, new_arr_dict)
(*let new_col_dict = Hashtbl.create 3
let new_arr_dict = Hashtbl.add new_col_dict "Names" 0; Hashtbl.create 10
let new_tab_dict = Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (new_col_dict, new_arr_dict);
  updated===new_tab_dict*)

(*Tests for insert*)
(*#1, inserting one element with one column/value*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Insert("People", ["Name"], [String "Jason"])) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (col_dict, new_arr_dict)
(*TEST_UNIT = print_string (dict_to_string (Hashtbl.find updated "People"));
  Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  print_string (dict_to_string (col_dict, new_arr_dict));
  updated===new_tab_dict*)

(*#2, inserting one element with more than one column/value*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Insert("People", ["Name"; "Age"], [String "Jason"; Int 10])) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"; Int 10|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (col_dict, new_arr_dict)
(*TEST_UNIT = print_string (dict_to_string (Hashtbl.find updated "People"));
  Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  print_string (dict_to_string (col_dict, new_arr_dict));
  updated===new_tab_dict*)

(*#3, inserting one element with some values, some nulls*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Insert("People", ["Name"], [String "Jason"])) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"; Null|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (col_dict, new_arr_dict)
(*TEST_UNIT = print_string (dict_to_string (Hashtbl.find updated "People"));
  Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  print_string (dict_to_string (col_dict, new_arr_dict));
  updated===new_tab_dict*)

(*Tests for drop*)
(*#1, dropping the only table*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"|];
  Hashtbl.add arr_dict 1 [|String "Jason"|]; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Drop("People")) tab_dict
let new_tab_dict = Hashtbl.create 3
TEST_UNIT = updated===new_tab_dict

(*#2, dropping one of two tables*)
let col_dict1 = Hashtbl.create 3
let arr_dict1 = Hashtbl.add col_dict1 "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict1 0 [|String "Bob"|]; Hashtbl.create 3
let col_dict2 = Hashtbl.create 3
let arr_dict2 = Hashtbl.add col_dict2 "Age" 0; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "Numbers" (col_dict2, arr_dict2);
  Hashtbl.add tab_dict "People" (col_dict1, arr_dict1);
  execute (Drop("Numbers")) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Bob"|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict1, new_arr_dict);
  updated===new_tab_dict

(*Tests for delete*)
(*#1, deleting the first element*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"|];
  Hashtbl.add arr_dict 1 [|String "Jason"|]; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Delete("People", Op (Eq ("Name", String "Bob")))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (col_dict, new_arr_dict)

(*#1, deleting the last element*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"|];
  Hashtbl.add arr_dict 1 [|String "Jason"|]; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Delete("People", Op (Eq ("Name", String "Jason")))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Bob"|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (col_dict, new_arr_dict)

(*#2, deleting multiple elements*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"; Int 12|];
  Hashtbl.add arr_dict 1 [|String "Jason"; Int 13|];
  Hashtbl.add arr_dict 2 [|String "Jeff"; Int 12|]; Hashtbl.create 3
let updated = Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Delete("People", Op (Eq ("Age", Int 12)))) tab_dict
let new_arr_dict = Hashtbl.create 3
let new_tab_dict = Hashtbl.add new_arr_dict 0 [|String "Jason"; Int 13|];
  Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict "People" (col_dict, new_arr_dict);
  dict_to_string (Hashtbl.find updated "People") ===
  dict_to_string (col_dict, new_arr_dict)

(*Tests for select, no unit tests, have to check printed values*)
let col_dict = Hashtbl.create 3
let arr_dict = Hashtbl.add col_dict "Name" 0;
  Hashtbl.add col_dict "Age" 1; Hashtbl.create 3
let tab_dict = Hashtbl.add arr_dict 0 [|String "Bob"; Int 123|];
  Hashtbl.add arr_dict 1 [|String "JJ"; Int 12|]; Hashtbl.create 3
(*#1, select all values*)
let updated = print_string "SELECT Name, Age FROM People\n";
  Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Select(["Name"; "Age"], "People", None)) tab_dict
(*#2, select only one value*)
let updated = print_string "\nSELECT Name, Age FROM People WHERE Name = Bob\n";
  Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Select(["Name"; "Age"], "People",
  Some (Op (Eq ("Name", String "Bob"))))) tab_dict
(*#3, select with constraint that no rows have*)
let updated = print_string "\nSELECT Name, Age FROM People WHERE Name = None\n";
  Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Select(["Name"; "Age"], "People",
  Some (Op (Eq ("Name", String "None"))))) tab_dict
(*#4, star case*)
let updated = print_string "\nSELECT * FROM People WHERE Name = Jason\n";
  Hashtbl.add tab_dict "People" (col_dict, arr_dict);
  execute (Select(["*"], "People",
  Some (Op (Eq ("Name", String "JJ"))))) tab_dict

let x = print_string "All tests run.\n"