let list_new (_ : Value.t list) =
  let list = ref [] in
  let list_add values =
    match values with
    | value :: _ ->
        list := value :: !list;
        Value.Unit
    | _ -> raise Error.unexpected_error
  in
  let list_get values =
    match values with
    | Value.IntValue index :: _ ->
        if index < 0 || index >= List.length !list then
          raise (Error.invalid_value (Value.IntValue index))
        else List.nth !list index
    | _ -> raise Error.unexpected_error
  in
  let obj = Hashtbl.create 2 in
  Hashtbl.add obj "add" (Value.ExternalFunction list_add);
  Hashtbl.add obj "get" (Value.ExternalFunction list_get);
  Value.Object obj

let register scope =
  Scope.set scope "list_new" (Value.ExternalFunction list_new)
