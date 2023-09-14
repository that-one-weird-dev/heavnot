open Heavnot

let undefined_variable id = raise (Failure ("Undefined variable " ^ id))

let cannot_invoke_non_function_type () =
  raise (Failure "Cannot invoke non function type")

let incompatible_type a b =
  raise
    (Failure
       ("Incompatible type " ^ Type.show a ^ " with " ^ Type.show b))

let rec check_node scope (node : Ast.t) : Type.t =
  let open Type in
  match node with
  | VariableDecl var -> (
      let value_type = check_node scope var.value in

      Scope.set scope var.identifier value_type;

      match (value_type, var.type_) with
      | value_type, None -> value_type
      | value_type, Some type_ -> (
          match Tcomp.equal scope value_type type_ with
          | Ok t -> t
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

      match Tcomp.equal scope type_ funct.return_type with
      | Ok t -> Function { params; return = t }
      | Error (a, b) -> raise (incompatible_type a b))
  | Literal (IntLiteral _) -> Type.Int
  | Literal (FloatLiteral _) -> Type.Float
  | Literal (StringLiteral _) -> Type.String
  | ObjectLiteral value ->
      Object (List.map (fun (id, value) -> (id, check_node scope value)) value)
  | VariableAccess id ->
      let type_ = Scope.get scope id in
      let type_ =
        match type_ with Some type_ -> type_ | None -> undefined_variable id
      in
      type_
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
