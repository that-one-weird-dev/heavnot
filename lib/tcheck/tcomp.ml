open Heavnot

let undefined_type id = raise (Failure ("Undefined type " ^ id))
let cant_compare_functions () = raise (Failure "Cant compare functions")

let get_type scope id =
  match Scope.get_type scope id with
  | Some type_ -> type_
  | None -> undefined_type id

let rec can_object_cast scope (from : (string * Type.t) list)
    (into : (string * Type.t) list) =
  if List.length from < List.length into then false
  else
    let delta =
      List.find_opt
        (fun (into_id, into_type) ->
          match
            List.find_opt
              (fun (from_id, from_type) ->
                String.equal from_id into_id
                && can_cast scope from_type into_type)
              from
          with
          | Some _ -> false
          | None -> true)
        into
    in
    match delta with Some _ -> false | None -> true

(* functions can cast only if equal*)
and can_function_cast scope a_params a_ret b_params b_ret =
  if List.length a_params != List.length b_params then false
  else
    let params_delta =
      List.find_opt
        (fun (a, b) -> can_cast scope a b)
        (List.combine a_params b_params)
    in
    let params_match =
      match params_delta with Some _ -> false | None -> true
    in
    let return_match = can_cast scope a_ret b_ret in

    params_match && return_match

and cast (scope : Scope.t) (from : Type.t) (into : Type.t) :
    (Type.t, Type.t * Type.t) result =
  let from = Tutils.dereference_type scope from in
  let into = Tutils.dereference_type scope into in

  match (from, into) with
  | Unit, Unit | Int, Int | Float, Float | String, String | Never, Never ->
      Ok into
  | Object obj_from, Object obj_into ->
      if can_object_cast scope obj_from obj_into then Ok into
      else Error (from, into)
  | Function func_from, Function func_into ->
      if
        can_function_cast scope func_from.params func_from.return
          func_into.params func_into.return
      then Ok into
      else Error (from, into)
  | _, Any -> Ok Any
  | from, into -> Error (from, into)

and can_cast scope a b =
  match cast scope a b with Ok _ -> true | Error _ -> false

and not_can_cast scope a b = not (can_cast scope a b)

and list_diff_opt (scope : Scope.t) (from : Type.t list) (into : Type.t list) =
  List.find_opt (fun (f, i) -> not_can_cast scope f i) (List.combine from into)


let smaller_cast (scope : Scope.t) (from : Type.t) (into : Type.t) =
    match cast scope from into with
    | Ok type_ -> Ok type_
    | Error (from, into) -> cast scope into from
