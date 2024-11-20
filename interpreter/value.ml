open Heavnot

type funct = { params : Ast.param list; return : Type.t; body : Ast.t list }
[@@deriving show]

type t =
  | Unit
  | Function of funct
  | IntValue of int
  | FloatValue of float
  | StringValue of string
  | BoolValue of bool
  | Object of (string, t) Hashtbl.t
  | Enum of { variant : string; value : t }
  | ExternalFunction of (t list -> t)

let unit () = Unit
let int value = IntValue value
let float value = FloatValue value
let string value = StringValue value
let external_function func = ExternalFunction func

let is_truty value =
  match value with
  | BoolValue value -> value
  | IntValue value -> value != 0
  | FloatValue value -> value < -0.0001 || value > 0.0001
  | StringValue value -> String.length value != 0
  | _ -> true

let rec show_object str (fields : (string * t) list) =
  let conc_str id value = str ^ id ^ ": " ^ show value in

  match fields with
  | (id, value) :: [] -> "{ " ^ conc_str id value ^ " }"
  | (id, value) :: fields -> show_object (conc_str id value ^ ", ") fields
  | [] -> "{}"

and show_function str ret params =
  let conc_str (param : Ast.param) =
    str ^ param.identifier ^ ": " ^ Type.show param.type_
  in

  match params with
  | param :: [] -> "(" ^ conc_str param ^ "): " ^ Type.show ret
  | param :: params -> show_function (conc_str param ^ ", ") ret params
  | [] -> "(): " ^ Type.show ret

and show_list str values =
  match values with
  | value :: [] -> "[" ^ str ^ show value ^ "]"
  | value :: values -> show_list (str ^ show value ^ ", ") values
  | [] -> "[]"

and show = function
  | Unit -> "()"
  | Function funct -> show_function "" funct.return funct.params
  | IntValue value -> string_of_int value
  | FloatValue value -> string_of_float value
  | StringValue value -> value
  | BoolValue value -> string_of_bool value
  | Object fields -> show_object "" (List.of_seq (Hashtbl.to_seq fields))
  | Enum enum -> enum.variant ^ "(" ^ show enum.value ^ ")"
  | ExternalFunction _ -> "EXTERNAL_FUNCTION"

let pp ppf value = Format.fprintf ppf "%s" (show value)
