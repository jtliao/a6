type wrapper =
  |Int of int
  |Float of float
  |String of string
  |Null

let wrap_to_string w =
  match w with
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | String s -> s
    | Null -> "null"

let read_file filename =
  let chan = open_in filename in
  let length = in_channel_length chan in
  let str = Bytes.create length in
  really_input chan str 0 length;
  close_in chan;
  str

let write_file changes filename =
  let chan = open_out filename in
  output_string chan changes;
  close_out chan

let string_to_wrap str =
  try
    let i = int_of_string str in
    Int i
  with
  | _ ->
    (try
      let fl = float_of_string str in
      Float fl
    with
    | _ -> if str = "null" then Null else String str)

let rec string_to_array str arr build =
  let char_at = Bytes.get str 0 in
  let rest = Bytes.sub str 1 ((Bytes.length str)-1 ) in
  match char_at with
    | '|' -> if build = "" then string_to_array rest arr ""
      else string_to_array rest (Array.append arr [|string_to_wrap build|]) ""
    | '\n' -> arr
    | _ -> string_to_array rest arr (build^(Bytes.make 1 char_at))

let string_to_dict s =
  failwith "TODO"

let array_to_string arr =
  Array.fold_left (fun acc x -> acc^(wrap_to_string x)^"|") "|" arr

let str_arr_to_string arr =
  Array.fold_left (fun acc x -> acc^x^"|") "|" arr

let hash_to_array tbl =
  let arr = Array.make (Hashtbl.length tbl) "Null" in
  Hashtbl.iter (fun k v -> Array.set arr v k) tbl;
  arr

let dict_to_string (t1,t2) =
  let dictstring = ref ((str_arr_to_string (hash_to_array t1))^"\n") in
  for i = 0 to (Hashtbl.length t2 - 1) do
    dictstring := !dictstring^(array_to_string (Hashtbl.find t2 i))^"\n"
  done;
  !dictstring