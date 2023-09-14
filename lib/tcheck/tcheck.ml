open Heavnot

let undefined_variable id = raise (Failure ("Undefined variable " ^ id))

let cannot_invoke_non_function_type () =
  raise (Failure "Cannot invoke non function type")

let incompatible_type a b =
  raise (Failure ("Incompatible type " ^ Type.show a ^ " with " ^ Type.show b))

let cannot_access_non_object type_ =
  Failure ("Cannot index into non object value of type " ^ Type.show type_)

let undefined_index_in_object id =
  Failure ("Undefined index " ^ id ^ " in object")

let undefined_type id = raise (Failure ("Undefined type " ^ id))

let rec dereference_type scope (type_ : Type.t) : Type.t =
  match type_ with
  | Reference id ->
      let type_ =
        match Scope.get_type scope id with
        | Some type_ -> type_
        | None -> raise (undefined_type id)
      in
      dereference_type scope type_
  | type_ -> type_

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
        match dereference_type scope type_ with
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

          match Tcomp.list_diff_opt scope funct.params param_types with
          | Some (a, b) -> incompatible_type a b
          | None -> funct.return)
      | _ -> cannot_invoke_non_function_type ())

and check_body scope (body : Ast.t list) : Type.t =
  match body with
  | node :: [] -> check_node scope node
  | node :: body ->
      ignore (check_node scope node);
      check_body scope body
  | [] -> Type.Unit

let check (root : Ast.root) =
  let scope = Scope.create None in

  List.iter (fun n -> ignore (check_node scope n)) root.body
