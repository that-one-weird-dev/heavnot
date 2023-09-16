type t =
  | Identifier of string
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | BoolLiteral of bool
  | ParenOpen
  | ParenClose
  | BracketOpen
  | BracketClose
  | BraceOpen
  | BraceClose
  | Equal
  | Comma
  | Colon
  | Dot
  | Semicolon
  | Pipe
  | Lesser
  | Greater
  | Underscore
  (* keywords *)
  | Unit
  | Int
  | Float
  | String
  | Bool
  | Fn
  | If
  | Else
  | Is
  | Match
[@@deriving show]

let from_identifier = function
  | "unit" -> Unit
  | "int" -> Int
  | "float" -> Float
  | "string" -> String
  | "bool" -> Bool
  | "true" -> BoolLiteral true
  | "false" -> BoolLiteral false
  | "fn" -> Fn
  | "if" -> If
  | "else" -> Else
  | "is" -> Is
  | "match" -> Match
  | id -> Identifier id
