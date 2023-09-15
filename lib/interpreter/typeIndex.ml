open Error
open Heavnot

let union_create params =
  match params with param :: _ -> param | [] -> raise unexpected_error
let union_create_ext_function = Value.ExternalFunction union_create

let index_type (type_ : Type.t) (_ : string) =
  match type_ with
  | Union _ -> union_create_ext_function
  | type_ -> raise (cannot_index_type type_)
