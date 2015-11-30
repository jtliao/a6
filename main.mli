open IOfile

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
type constraint =
  |Op of operator
  |And of operator * constraint
  |Or of operator * constraint

(*-Select (col, t, [None]) indicates the command SELECT col FROM t
  -Select (col, t, [Some constraint]) indicates the command SELECT col FROM
   t WHERE constraint
  -Update (t, [c1;c2], [v1;v2], cons) indicates the command UPDATE t SET c1=v1,
   c2=v2 WHERE cons
  -Delete (t, cons) is the command DELETE FROM t WHERE cons
  -Insert (t, [c1;c2], [v1;v2]) inserts v1 into c1, v2 into c2 in table t
  -Create (t, [c1;c2;...] creates table t with columns c1, c2,...)
  -Drop t drops the table t*)
type command =
  |Select of string list * string * constraint option
  |Update of string * string list * string list* constraint
  |Delete of string * constraint
  |Insert of string * string list * wrapper list
  |Create of string * string list
  |Drop of string

(*Parses the string that the user inputs into a type command*)
val parse_input : string -> command

(*Acts on the command that is inputted, taking the tuple of current
  database data structures (with the first hash table being the table name ->
  table values table, the second being the column name -> index table, and the
  third being the row index -> array table) as parameters and returning the
  updated pair after the command has been executed*)
val execute : command -> (('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t *
  ('e*'f) Hashtbl.t) -> (('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t *
  ('e*'f) Hashtbl.t)

(*The function that will act as the REPL, repreatedly running and passing
  in the current data structures as parameters, with the first hash table being
  the table name -> table values table, the second being the column name ->
  index table, and the third being the row index -> array table*)
val run_repl : (('a*'b) Hashtbl.t * ('c*'d) Hashtbl.t * ('e*'f) Hashtbl.t)->unit

(*The "main" function that will ask for the database file name and import
  it using iofile.ml, then run the REPL*)
val _ : unit