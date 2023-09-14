open Error
open Heavnot
module Scope = Scope

let rec check_node scope (node : Ast.t) : Type.t =
  let open Type in
  match node with
  | VariableDecl var -> (
      let value_type = check_node scope var.value in

      match (value_type, var.type_) with
      | value_type, None ->
          Scope.set scope var.identifier value_type;
          value_type
      | value_type, Some type_ -> (
          match Tcomp.cast scope value_type type_ with
          | Ok type_ ->
              Scope.set scope var.identifier type_;
              type_
          | Error (a, b) -> raise (incompatible_type a b)))
  | TypeDecl decl ->
      Scope.set_type scope decl.identifier decl.type_;
      Unit
  | Function funct -> (
      let params = List.map (fun (p : Ast.param) -> p.type_) funct.params in

      let funct_scope = Scope.create (Some scope) in
      List.iter
        (fun (p : Ast.param) -> Scope.set funct_scope p.identifier p.type_)
        funct.params;

      let type_ = check_body funct_scope funct.body in

      match Tcomp.cast scope type_ funct.return_type with
      | Ok t -> Function { params; return = t }
      | Error (a, b) -> raise (incompatible_type a b))
  | UnitLiteral -> Type.Unit
  | IntLiteral _ -> Type.Int
  | FloatLiteral _ -> Type.Float
  | StringLiteral _ -> Type.String
  | ObjectLiteral value ->
      Object (List.map (fun (id, value) -> (id, check_node scope value)) value)
  | VariableAccess id ->
      let type_ = Scope.get scope id in
      let type_ =
        match type_ with Some type_ -> type_ | None -> undefined_variable id
      in
      type_
  | ObjectAccess acc -> (
      let type_ = check_node scope acc.value in
      let obj =
        match Tutils.dereference_type scope type_ with
        | Object obj -> obj
        | type_ -> raise (cannot_access_non_object type_)
      in

      let field_obj =
        List.find_opt (fun (id, _) -> String.equal id acc.identifier) obj
      in
      match field_obj with
      | Some (_, type_) -> type_
      | None -> raise (undefined_index_in_object acc.identifier))
  | FunctionCall call -> (
      let funct = check_node scope call.value in
      match funct with
      | Function funct -> (
          let param_types =
            List.map (fun (n : Ast.t) -> check_node scope n) call.params
          in

          match Tcomp.list_diff_opt scope param_types funct.params with
          | Some (a, b) -> incompatible_type a b
          | None -> funct.return)
      | _ -> cannot_invoke_non_function_type ())
  | IfExpression expr -> (
      ignore (check_node scope expr.condition);

      let then_scope = Scope.create (Some scope) in
      let then_result = check_body then_scope expr.then_body in
      let else_scope = Scope.create (Some scope) in
      let else_result = check_body else_scope expr.else_body in

      match Tcomp.smaller_cast scope then_result else_result with
      | Ok type_ -> type_
      | Error (from, into) -> raise (incompatible_type from into))

and check_body scope (body : Ast.t list) : Type.t =
  match body with
  | node :: [] -> check_node scope node
  | node :: body ->
      ignore (check_node scope node);
      check_body scope body
  | [] -> Type.Unit

let check_root scope (root : Ast.root) =
  List.iter (fun n -> ignore (check_node scope n)) root.body
