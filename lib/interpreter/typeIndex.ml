open Error
open Heavnot

let union_create id params =
    match params with param :: _ -> Value.Union { variant = id; value = param } | [] -> raise unexpected_error
let union_create_ext_function id = Value.ExternalFunction (union_create id)

let index_type (type_ : Type.t) (identifier : string) =
  match type_ with
  | Union _ -> union_create_ext_function identifier
  | type_ -> raise (cannot_index_type type_)
