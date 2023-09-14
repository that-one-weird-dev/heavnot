type t =
  | Unit
  | Never
  | Int
  | Float
  | String
  | Object of (string * t) list
  | Function of { params : t list; return : t }
  | Reference of string

let rec show_object str (fields : (string * t) list) =
  let conc_str id type_ = str ^ id ^ ": " ^ (show type_) in

  match fields with
    | (id, type_) :: [] -> "{ " ^ conc_str id type_ ^ " }"
  | (id, type_) :: fields -> show_object ((conc_str id type_) ^ ", ") fields
  | [] -> "{}"

and show = function
  | Unit -> "()"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Object fields -> show_object "" fields
  | Function _ -> "function()"
  | Never -> "never"
  | Reference id -> id

let pp ppf value = Format.fprintf ppf "%s" (show value)
