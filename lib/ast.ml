type param = { identifier : string; type_ : Type.t } [@@deriving show]

type t =
  | VariableDecl of { identifier : string; type_ : Type.t option; value : t }
  | TypeDecl of { identifier : string; type_ : Type.t }
  | Function of { params : param list; return_type : Type.t; body : t list }
  | UnitLiteral
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | BoolLiteral of bool
  | ObjectLiteral of (string * t) list
  | VariableAccess of string
  | ObjectAccess of { value : t; identifier : string }
  | FunctionCall of { value : t; params : t list }
  | IfExpression of { condition : t; then_body : t list; else_body : t list }
[@@deriving show]

type root = { body : t list }

let rec indentation_of str ind =
  if ind > 0 then indentation_of (str ^ "  ") (ind - 1) else str

let print_indented str ind =
  print_string (indentation_of "" ind);
  print_endline str

let rec print_node ind ast =
  let list_body body ind = List.iter (fun a -> print_node ind a) body in

  match ast with
  | Function func ->
      print_indented "Function:" ind;

      print_indented "params:" (ind + 1);
      List.iter (fun p -> print_indented p.identifier (ind + 2)) func.params;

      print_indented "body:" (ind + 1);
      list_body func.body (ind + 2)
  | VariableDecl var ->
      print_indented ("Variable(" ^ var.identifier ^ "):") ind;
      print_node (ind + 1) var.value
  | TypeDecl decl -> print_indented ("Type(" ^ decl.identifier ^ ")") ind
  | UnitLiteral -> print_indented "UnitLiteral" ind
  | IntLiteral value ->
      print_indented ("IntLiteral(" ^ string_of_int value ^ ")") ind
  | FloatLiteral value ->
      print_indented ("Literal(" ^ string_of_float value ^ ")") ind
  | StringLiteral value -> print_indented ("StringLiteral(" ^ value ^ ")") ind
  | BoolLiteral value ->
      print_indented ("BoolLiteral(" ^ string_of_bool value ^ ")") ind
  | ObjectLiteral _ -> print_indented "ObjectLiteral" ind
  | VariableAccess id -> print_indented ("VariableAccess(" ^ id ^ ")") ind
  | ObjectAccess acc ->
      print_indented ("ObjectAccess(" ^ acc.identifier ^ "):") ind;
      print_node (ind + 1) acc.value
  | FunctionCall call ->
      print_indented "FunctionCall:" ind;
      print_node (ind + 1) call.value
  | IfExpression call ->
      print_indented "IfExpression:" ind;

      print_indented "condition:" (ind + 1);
      print_node (ind + 2) call.condition;

      print_indented "then:" (ind + 1);
      list_body call.then_body (ind + 2);

      print_indented "else:" (ind + 1);
      list_body call.else_body (ind + 2)

let print_node = print_node 0
let print_root root = List.iter print_node root.body
