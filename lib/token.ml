type t =
  | Identifier of string
  | Literal of Literal.t
  | ParenOpen
  | ParenClose
  | BracketOpen
  | BracketClose
  | BraceOpen
  | BraceClose
  | Equal
  | Comma
  | Colon
  (* keywords *)
  | Int
  | Float
  | String
[@@deriving show]

let from_identifier = function
  | "int" -> Int
  | "float" -> Float
  | "string" -> String
  | id -> Identifier id
