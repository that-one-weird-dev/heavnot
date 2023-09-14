
type param = {
  identifier : string;
  type_ : Type.t;
}
[@@deriving show]

type t =
  | VariableDecl of { identifier : string; type_: Type.t option; value : t; }
  | Function of { params : param list; return_type : Type.t; body : t list; }
  | Literal of Literal.t
  | VariableAccess of string
  | FunctionCall of { value : t; params : t list }
[@@deriving show]

type root = {
  body: t list
}


let rec indentation_of str ind =
  if ind > 0 then indentation_of (str ^ "  ") (ind - 1)
  else str
;;

let print_indented str ind =
  print_string (indentation_of "" ind);
  print_endline str
;;

let rec print_node ind ast =
  let list_body body ind =
    List.iter (fun a -> print_node ind a) body
  in

  match ast with
  | Function func ->
      print_indented ("Function:") ind;

      print_indented "params:" (ind + 1);
      List.iter (fun p -> print_indented p.identifier (ind + 2)) func.params;

      print_indented "body:" (ind + 1);
      list_body func.body (ind + 2);
  | VariableDecl var ->
      print_indented ("Variable(" ^ var.identifier ^ "):") ind;
      print_node (ind + 1) var.value
  | Literal value ->
      print_indented ("Literal(" ^ (Literal.show value) ^ ")") ind
  | VariableAccess id ->
      print_indented ("VariableAccess(" ^ id ^ ")") ind
  | FunctionCall call ->
      print_indented "FunctionCall:" ind;
      print_node (ind + 1) call.value
;;

let print_node = print_node 0
let print_root root = List.iter print_node root.body