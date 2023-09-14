type 'a obj = (string, 'a) Hashtbl.t

type t =
  | Unit
  | Never
  | Int
  | Float
  | String
  | Object of t obj
  | Function of { params : t list; return : t }
  | Reference of string

let to_string = function
  | Unit -> "()"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Object _ -> "{object}"
  | Function _ -> "function()"
  | Never -> "never"
  | Reference id -> id

let pp ppf value = Format.fprintf ppf "%s" (to_string value)
