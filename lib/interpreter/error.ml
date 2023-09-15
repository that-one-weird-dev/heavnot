open Heavnot

let unexpected_error = Failure "Unexpected error"
let cannot_index_type type_ = Failure ("Cannot index type " ^ Type.show type_)
let undefined_type id = raise (Failure ("Undefined type " ^ id))

let undefined_type_index type_ identifier =
  Failure ("Undefined index " ^ identifier ^ " in type " ^ Type.show type_)
