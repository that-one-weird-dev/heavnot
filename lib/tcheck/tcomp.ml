open Heavnot

let undefined_type id = raise (Failure ("Undefined type " ^ id))
let cant_compare_functions () = raise (Failure "Cant compare functions")

let get_type scope id =
  match Scope.get_type scope id with
  | Some type_ -> type_
  | None -> undefined_type id

[@@@warning "-27"]

let rec object_equal scope (a : (string * Type.t) list)
    (b : (string * Type.t) list) =
  let delta =
    List.find_opt
      (fun ( ((a_id : string), (a_type : Type.t)),
             ((b_id : string), (b_type : Type.t)) ) ->
        (not (String.equal a_id b_id)) || not_equal_bool scope a_type b_type)
      (List.combine a b)
  in
  match delta with Some _ -> false | None -> true

and equal (scope : Scope.t) (a : Type.t) (b : Type.t) :
    (Type.t, Type.t * Type.t) result =
  match (a, b) with
  | Unit, Unit | Int, Int | Float, Float | String, String | Never, Never -> Ok a
  | Object obj_a, Object obj_b ->
      if object_equal scope obj_a obj_b then Ok a else Error (a, b)
  | Reference a, Reference b ->
      equal scope (get_type scope a) (get_type scope b)
  | Reference a, b -> equal scope (get_type scope a) b
  | a, Reference b -> equal scope a (get_type scope b)
  | a, b -> Error (a, b)

and equal_bool scope a b =
  match equal scope a b with Ok _ -> true | Error _ -> false

and not_equal_bool scope a b = not (equal_bool scope a b)

and list_diff_opt (scope : Scope.t) (a : Type.t list) (b : Type.t list) =
  List.find_opt (fun (a, b) -> not_equal_bool scope a b) (List.combine a b)