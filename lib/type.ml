type t =
  | Unit
  | Int
  | Float
  | String
  | Object of (string, t) Hashtbl.t
  | Function of { params : t list; return : t }
  | Never

let to_string = function
  | Unit -> "()"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Object _ -> "{object}"
  | Function _ -> "function()"
  | Never -> "never"

let pp ppf value = Format.fprintf ppf "%s" (to_string value)

let list_equals (a : t list) (b : t list) =
  let different = List.find_opt (fun (a, b) -> a != b) (List.combine a b) in
  match different with Some _ -> false | None -> true
