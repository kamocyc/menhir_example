
type value =
  | Object of (string * value) list
  | Float of float
  | Null

let rec show_assoc (s, v) = s ^ " : " ^ show_object v
and show_object v = match v with
  | Object assocs -> "{" ^ (List.map show_assoc assocs |> String.concat ", ") ^ "}"
  | Float f -> string_of_float f
  | Null -> "null"
  