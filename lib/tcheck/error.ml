open Heavnot

let unexpected_error = Failure "Unexpected error"
let undefined_variable id = raise (Failure ("Undefined variable " ^ id))
let undefined_type id = raise (Failure ("Undefined type " ^ id))

let cannot_invoke_non_function_type () =
  raise (Failure "Cannot invoke non function type")

let incompatible_type a b =
  raise (Failure ("Incompatible type " ^ Type.show a ^ " with " ^ Type.show b))

let cannot_access_non_object type_ =
  Failure ("Cannot index into non object value of type " ^ Type.show type_)

let undefined_index_in_object id =
  Failure ("Undefined index " ^ id ^ " in object")

let cannot_index_type type_ = Failure ("Cannot index type " ^ Type.show type_)
let undefined_type_index type_ identifier = Failure ("Undefined index " ^ identifier ^ " in type " ^ Type.show type_)

let cant_compare_functions () = raise (Failure "Cant compare functions")

let cannot_match_non_union type_ =
  Failure ("Cannot match non union type " ^ Type.show type_)
