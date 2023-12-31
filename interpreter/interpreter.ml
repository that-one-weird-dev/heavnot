open Error
open Heavnot
module Value = Value
module Scope = Scope

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
  | FunctionDecl funct ->
      let value_funct : Value.funct =
        { params = funct.params; return = funct.return_type; body = funct.body }
      in
      let value = Value.Function value_funct in
      Scope.set scope funct.identifier value;
      Value.Unit
  | VariableDecl var ->
      let value = exec_node scope var.value in
      Scope.set scope var.identifier value;
      Value.Unit
  | TypeDecl decl ->
      Scope.set_type scope decl.identifier decl.type_;
      Unit
  | UnitLiteral -> Unit
  | IntLiteral value -> IntValue value
  | FloatLiteral value -> FloatValue value
  | StringLiteral value -> StringValue value
  | BoolLiteral value -> BoolValue value
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
  | TypeAccess acc ->
      let type_ = Tutils.dereference_type scope acc.type_ in
      TypeIndex.index_type type_ acc.identifier
  | FunctionCall call -> (
      let value = exec_node scope call.value in
      let values = List.map (exec_node scope) call.params in

      match value with
      | Function funct -> exec_function scope funct values
      | ExternalFunction funct -> funct values
      | value -> invalid_value value)
  | IfExpression expr ->
      let condition_value = exec_node scope expr.condition in
      let if_scope = Scope.create (Some scope) in

      if Value.is_truty condition_value then exec_body if_scope expr.then_body
      else exec_body if_scope expr.else_body
  | MatchExpression expr -> (
      let value = exec_node scope expr.value in
      let variant, value =
        match value with
        | Enum enum -> (enum.variant, enum.value)
        | value -> raise (cannot_match_non_union value)
      in
      let branch =
        List.find_opt
          (fun (branch : Ast.match_branch) ->
            match branch.variant with
            | Ast.MatchIdentifier { identifier; var_identifier = _ } ->
                String.equal identifier variant
            | MatchDefault -> true)
          expr.branches
      in
      match branch with
      | Some branch ->
          let match_scope = Scope.create (Some scope) in
          (match branch.variant with
          | Ast.MatchIdentifier id ->
              Scope.set match_scope id.var_identifier value
          | MatchDefault -> ());
          exec_body match_scope branch.body
      | None -> raise unexpected_error)

let create_scope () =
  let scope = Scope.create None in
  Intrinsics.register scope;
  scope

let execute_root scope (root : Ast.root) = ignore (exec_body scope root.body)

let execute_function scope id =
  let value = Scope.get scope id in
  match value with
  | Some (Function funct) -> exec_function scope funct []
  | _ -> raise (Failure ("No function named " ^ id))
