type t =
  | Unit
  | Never
  | Int
  | Float
  | String
  | Bool
  | Object of (string * t) list
  | Union of (string * t) list
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

and show_union str variants =
  let conc_str (id : string) (type_ : t) = str ^ id ^ ": " ^ show type_ in

  match variants with
  | (id, type_) :: [] -> "| " ^ conc_str id type_
  | (id, type_) :: variants -> show_union (conc_str id type_ ^ " | ") variants
  | [] -> "| "

and show = function
  | Unit -> "()"
  | Never -> "never"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Object fields -> show_object "" fields
  | Union variants -> show_union "" variants
  | Function funct -> show_function "" funct.return funct.params
  | Reference id -> id

let pp ppf value = Format.fprintf ppf "%s" (show value)
