type t =
  | Never
  | Any
  | Unit
  | Int
  | Float
  | String
  | Bool
  | Object of (string * t) list
  | Function of { params : t list; return : t }
  | Reference of string

let rec show_object str (fields : (string * t) list) =
  let conc_str id type_ = str ^ id ^ ": " ^ show type_ in

  match fields with
  | (id, type_) :: [] -> "{ " ^ conc_str id type_ ^ " }"
  | (id, type_) :: fields -> show_object (conc_str id type_ ^ ", ") fields
  | [] -> "{}"

and show_function str ret params =
  let conc_str (param : t) = str ^ show param in

  match params with
  | param :: [] -> "(" ^ conc_str param ^ "): " ^ show ret
  | param :: params -> show_function (conc_str param ^ ", ") ret params
  | [] -> "(): " ^ show ret

and show = function
  | Never -> "never"
  | Any -> "any"
  | Unit -> "()"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Object fields -> show_object "" fields
  | Function funct -> show_function "" funct.return funct.params
  | Reference id -> id

let pp ppf value = Format.fprintf ppf "%s" (show value)
