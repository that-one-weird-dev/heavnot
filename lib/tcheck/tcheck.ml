open Heavnot

let undefined_variable id = raise (Failure ("Undefined variable " ^ id))
let cannot_invoke_non_function_type () = raise (Failure ("Cannot invoke non function type"))
let incompatible_type () = raise (Failure "Incompatible type")

let rec check_node scope (node : Ast.t) : Type.t =
  let open Type in

  match node with
  | Variable var ->
      let value_type = check_node scope var.value in

      Scope.set scope var.identifier value_type;

      (match (value_type, var.type_) with
      | value_type, None -> value_type
      | value_type, Some type_ ->
          if value_type != type_ then incompatible_type ()
          else value_type)

  | Function funct ->
      let params = List.map (fun (p : Ast.param) -> p.type_) funct.params in

      let funct_scope = Scope.create (Some scope) in
      List.iter (fun (p : Ast.param) -> Scope.set funct_scope p.identifier p.type_) funct.params;

      let type_ = check_body funct_scope funct.body in

      if type_ == funct.return_type then
        Function { params; return = funct.return_type }
      else incompatible_type ()

  | Literal IntLiteral _ -> Type.Int
  | Literal FloatLiteral _ -> Type.Float
  | Literal StringLiteral _ -> Type.String

  | VariableAccess id ->
      let type_ = Scope.get scope id in
      let type_ = match type_ with
      | Some type_ -> type_
      | None -> undefined_variable id
      in
      type_

  | FunctionCall call ->
      let funct = check_node scope call.value in
      (match funct with
      | Function funct ->
          let param_types = List.map (fun (n : Ast.t) -> check_node scope n) call.params in

          if (Type.list_equals funct.params param_types) then
              funct.return
          else incompatible_type ()
      | _ -> cannot_invoke_non_function_type ())


and check_body scope (body : Ast.t list) : Type.t =
  match body with
  | node :: [] ->
      check_node scope node
  | node :: body ->
      ignore (check_node scope node);
      check_body scope body
  | [] -> Type.Unit
;;


let check (root : Ast.root) =
  let scope = Scope.create None in

  List.iter (fun n -> ignore (check_node scope n)) root.body
;;
