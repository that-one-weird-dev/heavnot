open Heavnot

let unexpected_error = Failure "Unexpected error"
let cannot_index_type type_ = Failure ("Cannot index type " ^ Type.show type_)
let undefined_type id = raise (Failure ("Undefined type " ^ id))

let undefined_type_index type_ identifier =
  Failure ("Undefined index " ^ identifier ^ " in type " ^ Type.show type_)

let undefined_variable id = raise (Failure ("Undefined variable " ^ id))

let invalid_value found =
  raise
    (Failure
       ("Expected value of type " ^ "**TODO**" ^ " found " ^ Value.show found
      ^ " instead"))

let invalid_arg_count_on_function_call () =
  raise (Failure "Invalid arg count on function call")

let cannot_access_non_object value =
  Failure ("Cannot index into non object value of type " ^ Value.show value)

let cannot_match_non_union value =
  Failure ("Cannot match non union value " ^ Value.show value)
