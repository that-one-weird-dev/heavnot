let unexpected_error = Failure "Unexpected error"
let list_new (_ : Value.t list) = Value.List { values = [] }

let list_add values =
  match values with
  | Value.List list :: value :: _ ->
      list.values <- value :: list.values;
      Value.Unit
  | _ -> raise unexpected_error

let register scope =
  Scope.set scope "list_new" (Value.ExternalFunction list_new);
  Scope.set scope "list_add" (Value.ExternalFunction list_add)
