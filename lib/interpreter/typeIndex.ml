open Error
open Heavnot

let union_create id params =
  match params with
  | param :: _ -> Value.Union { variant = id; value = param }
  | [] -> raise unexpected_error

let union_create_ext_function variants identifier =
  let prop =
    List.find_opt (fun (id, _) -> String.equal id identifier) variants
  in

  let prop_type =
    match prop with
    | Some (_, prop_type) -> prop_type
    | None -> raise (undefined_type_index (Union variants) identifier)
  in

  match prop_type with
  | Unit -> Value.Union { variant = identifier; value = Value.Unit }
  | _ -> Value.ExternalFunction (union_create identifier)

let index_type (type_ : Type.t) (identifier : string) =
  match type_ with
  | Union variants -> union_create_ext_function variants identifier
  | type_ -> raise (cannot_index_type type_)
