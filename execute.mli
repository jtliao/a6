open Iofile

(*Type to represent the different possibilities for constraints in the WHERE
  statement, where string is the field name and wrapper is the type of
  the element*)
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
  |And of constr * constr
  |Or of constr * constr

(*-Select (col, t, [None]) indicates the command SELECT col FROM t
  -Select (col, t, [Some constraint]) indicates the command SELECT col FROM
   t WHERE constraint
  -Update (t, [c1;c2], [v1;v2], cons) indicates the command UPDATE t SET c1=v1,
   c2=v2 WHERE cons
  -Delete (t, cons) is the command DELETE FROM t WHERE cons
  -Insert (t, [c1;c2], [v1;v2]) inserts v1 into c1, v2 into c2 in table t
  -Create (t, [c1;c2;...] creates table t with columns c1, c2,...)
  -Drop t drops the table t
  -Open t opens the existing table with name t*)
type command =
  |Select of string list * string * constr option
  |Update of string * string list * wrapper list * constr
  |Delete of string * constr
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string
  |Open of string

(*Acts on the command that is inputted, taking the tuple of current
  database data structures (with the hash table mapping from the table name to
  table values) as parameters and returning the
  updated pair after the command has been executed*)
val execute : command -> (string, (string, int) Hashtbl.t *
  (int, wrapper array) Hashtbl.t) Hashtbl.t ->
  (string, (string, int) Hashtbl.t * (int, wrapper array) Hashtbl.t) Hashtbl.t