open Main
open Iofile
open Assertions

(*
type operator =
  |Eq of string * wrapper
  |Lt of string * wrapper
  |Gt of string * wrapper
  |LtEq of string * wrapper
  |GtEq of string * wrapper
  |NotEq of string * wrapper

(*Type to create a constraint using a single operator, an And combining two
  operators or more, or an Or combining two operators or more*)
type constr =
  |Op of operator
  |And of operator * constr
  |Or of operator * constr

(*-Update (t, [c1;c2], [v1;v2], cons) indicates the command UPDATE t SET c1=v1,
   c2=v2 WHERE cons
  -Delete (t, cons) is the command DELETE FROM t WHERE cons
  -Insert (t, [c1;c2], [v1;v2]) inserts v1 into c1, v2 into c2 in table t
  -Create (t, [c1;c2;...] creates table t with columns c1, c2,...)
  -Drop t drops the table t*)
type command =
  |Update of string * string list * wrapper list * constr
  |Delete of string * constr
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

---this stuff above is just for reference, will delete later*)

(*****************************UNIT TESTS FOR MAIN*****************************)

(*Tests for update*)
(*1*)
let col_dict1 = Hashtbl.create 3
let arr_dict1 = Hashtbl.add col_dict1 "Name" 0; Hashtbl.create 3
let tab_dict1 = Hashtbl.add arr_dict1 0 [|String "Bob"|];Hashtbl.create 3
let updated1 = Hashtbl.add tab_dict1 "People" (col_dict1, arr_dict1); execute (Update("People", ["Name"], [String "Jason"],
  Op (Eq ("Name", String "Bob")))) tab_dict1

let new_arr_dict1 = Hashtbl.create 3
let new_tab_dict1 = Hashtbl.add new_arr_dict1 0 [|String "Jason"|];Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict1 "People" (col_dict1, new_arr_dict1);
  updated1===new_tab_dict1


let col_dict2 = Hashtbl.create 3
let arr_dict2 = Hashtbl.add col_dict2 "Name" 0; Hashtbl.create 3
let tab_dict2 = Hashtbl.add arr_dict2 0 [|String "Bob"|];Hashtbl.create 3
let updated2 = Hashtbl.add tab_dict2 "People" (col_dict2, arr_dict2); execute (Update("People", ["Name"], [String "Jason"],
  Op (Eq ("Name", String "Bob")))) tab_dict2

let new_arr_dict2 = Hashtbl.create 3
let new_tab_dict2 = Hashtbl.add new_arr_dict2 0 [|String "Jason"|];Hashtbl.create 3
TEST_UNIT = Hashtbl.add new_tab_dict2 "People" (col_dict2, new_arr_dict2);
  updated2===new_tab_dict2


let x = print_string "All tests pass!\n"