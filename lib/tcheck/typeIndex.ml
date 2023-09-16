open Error
open Heavnot

let index_union variants identifier =
  let prop =
    List.find_opt (fun (id, _) -> String.equal id identifier) variants
  in

  let prop_type =
    match prop with
    | Some (_, prop_type) -> prop_type
    | None -> raise (undefined_type_index (Union variants) identifier)
  in

  match prop_type with
  | Type.Unit -> Type.Union variants
  | prop_type ->
      Type.Function { params = [ prop_type ]; return = Union variants }

let index_type (type_ : Type.t) identifier =
  match type_ with
  | Union variants -> index_union variants identifier
  | type_ -> raise (cannot_index_type type_)
