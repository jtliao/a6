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