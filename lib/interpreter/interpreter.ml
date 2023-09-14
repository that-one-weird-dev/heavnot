open Heavnot
module Scope = Scope
module Value = Value

let undefined_variable id = raise (Failure ("Undefined variable " ^ id))

let invalid_value found =
  raise
    (Failure
       ("Expected value of type " ^ "**TODO**" ^ " found " ^ Value.show found
      ^ " instead"))

let invalid_arg_count_on_function_call () =
  raise (Failure "Invalid arg count on function call")

let cannot_access_non_object value =
  Failure ("Cannot index into non object value of type " ^ Value.show value)

let rec exec_function (scope : Scope.t) (funct : Value.funct)
    (param_values : Value.t list) : Value.t =
  let scope = Scope.create (Some scope) in

  if List.length funct.params != List.length param_values then
    invalid_arg_count_on_function_call ()
  else
    let params = List.combine funct.params param_values in
    List.iter
      (fun ((p : Ast.param), v) -> Scope.set scope p.identifier v)
      params;

    exec_body scope funct.body

and exec_body (scope : Scope.t) (body : Ast.t list) : Value.t =
  match body with
  | node :: [] -> exec_node scope node
  | node :: body ->
      ignore (exec_node scope node);
      exec_body scope body
  | [] -> Unit

and exec_node (scope : Scope.t) (node : Ast.t) : Value.t =
  match node with
  | Function funct ->
      let value_funct : Value.funct =
        { params = funct.params; return = funct.return_type; body = funct.body }
      in
      Value.Function value_funct
  | VariableDecl var ->
      let value = exec_node scope var.value in
      Scope.set scope var.identifier value;
      value
  | TypeDecl decl ->
      Scope.set_type scope decl.identifier decl.type_;
      Unit
  | UnitLiteral -> Unit
  | IntLiteral value -> IntValue value
  | FloatLiteral value -> FloatValue value
  | StringLiteral value -> StringValue value
  | ObjectLiteral value ->
      let obj = Hashtbl.create 784593 in
      List.iter (fun (id, t) -> Hashtbl.add obj id (exec_node scope t)) value;
      Object obj
  | VariableAccess id ->
      let value = Scope.get scope id in
      let value =
        match value with Some value -> value | None -> undefined_variable id
      in
      value
  | ObjectAccess acc ->
      let value = exec_node scope acc.value in
      let obj =
        match value with
        | Object obj -> obj
        | value -> raise (cannot_access_non_object value)
      in

      Hashtbl.find obj acc.identifier
  | FunctionCall call ->
      let value = exec_node scope call.value in
      let funct =
        match value with
        | Function funct -> funct
        | value -> invalid_value value
      in

      let values = List.map (exec_node scope) call.params in

      exec_function scope funct values

let execute (root : Ast.root) =
  let scope = Scope.create None in

  ignore (exec_body scope root.body);
  scope

let execute_function scope id =
  let value = Scope.get scope id in
  match value with
  | Some (Function funct) -> exec_function scope funct []
  | _ -> raise (Failure ("No function named " ^ id))
