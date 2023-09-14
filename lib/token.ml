type t =
  | Identifier of string
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
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
  (* keywords *)
  | Int
  | Float
  | String
  | Fn
[@@deriving show]

let from_identifier = function
  | "int" -> Int
  | "float" -> Float
  | "string" -> String
  | "fn" -> Fn
  | id -> Identifier id
