
let rec dereference_type scope (type_ : Heavnot.Type.t) : Heavnot.Type.t =
  match type_ with
  | Reference id ->
      let type_ =
        match Scope.get_type scope id with
        | Some type_ -> type_
        | None -> raise (Error.undefined_type id)
      in
      dereference_type scope type_
  | type_ -> type_
