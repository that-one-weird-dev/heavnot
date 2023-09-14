open Heavnot

type funct = { params : Ast.param list; body : Ast.t list } [@@deriving show]

type t =
  | Unit
  | Function of funct
  | IntValue of int
  | FloatValue of float
  | StringValue of string
[@@deriving show]
