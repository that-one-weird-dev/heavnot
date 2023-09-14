open Heavnot

type funct = { params : Ast.param list; return : Type.t; body : Ast.t list } [@@deriving show]

type t =
  | Unit
  | Function of funct
  | IntValue of int
  | FloatValue of float
  | StringValue of string
  | Object of (string, t) Hashtbl.t

let rec show_object str (fields : (string * t) list) =
  let conc_str id value = str ^ id ^ ": " ^ show value in

  match fields with
  | (id, value) :: [] -> "{ " ^ conc_str id value ^ " }"
  | (id, value) :: fields -> show_object (conc_str id value ^ ", ") fields
  | [] -> "{}"

and show_function str ret params =
  let conc_str (param : Ast.param) = str ^ param.identifier ^ ": " ^ Type.show param.type_ in

  match params with
  | param :: [] -> "(" ^ conc_str param ^ "): " ^ Type.show ret
  | param :: params -> show_function (conc_str param ^ ", ") ret params
  | [] -> "(): " ^ Type.show ret

and show = function
  | Unit -> "()"
  | Function funct -> show_function "" funct.return funct.params
  | IntValue value -> string_of_int value
  | FloatValue value -> string_of_float value
  | StringValue value -> value
  | Object fields -> show_object "" (List.of_seq (Hashtbl.to_seq fields))

let pp ppf value = Format.fprintf ppf "%s" (show value)
